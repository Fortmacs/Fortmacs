! test_js_fm.f90 -- tests for JavaScript mode (js_fm).
!
! Copyright (C) 2017-2026 Free Software Foundation, Inc.
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

!> Ports the tests of test/lisp/progmodes/js-tests.el that use what is
!> ported (the fill tests and the JSX indentation tests wait for those;
!> the resource files are embedded, and their file-local variables are
!> set by the tests), with own tests of the syntactic context and the
!> Font Lock levels.
module test_js_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use buffer_fm, only: get_buffer_create, set_buffer, erase_buffer, kill_buffer
    use insdel_fm, only: insert
    use lisp_fm, only: value_type, nilp, consp, symbolp, xsymbol, list_elements, symbols
    use lread_fm, only: sym
    use editfns_fm, only: point, point_min, point_max, goto_char, widen, bobp, eobp, buffer_substring
    use cmds_fm, only: forward_line
    use simple_fm, only: back_to_indentation
    use indent_fm, only: indent_region, current_column, indent_tabs_mode
    use search_fm, only: re_search_forward, replace_match, match_data, set_match_data, SEARCH_NOERROR
    use syntax_fm, only: parse_state_type, syntax_ppss
    use textprop_fm, only: get_text_property
    use font_lock_fm, only: font_lock_ensure
    use modes_fm, only: minor_modes
    use lisp_cmds_fm, only: beginning_of_defun, end_of_defun
    use bindings_fm, only: init_bindings
    use js_fm, only: js_mode, js_indent_line, js_syntactic_context, js_indent_level, js_chain_indent, &
                     js_indent_align_list_continuation, js_indent_first_init, JS_INDENT_FIRST_INIT_NIL, &
                     JS_INDENT_FIRST_INIT_T, JS_INDENT_FIRST_INIT_DYNAMIC, JS_CONTEXT_TOPLEVEL, JS_CONTEXT_FUNCTION
    implicit none(type, external)
    private

    public :: collect_js_fm

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

    ! js-chain.js.
    character(len=*), parameter :: R_JS_CHAIN = &
                                   "// Normal chaining."//LF &
                                   //"let x = svg.mumble()"//LF &
                                   //"           .zzz;"//LF &
                                   //""//LF &
                                   //"// Chaining with an intervening line comment."//LF &
                                   //"let x = svg.mumble()"//TAB//""//TAB//"// line comment"//LF &
                                   //"           .zzz;"//LF &
                                   //""//LF &
                                   //"// Chaining with multiple dots."//LF &
                                   //"let x = svg.selectAll().something()"//LF &
                                   //"           .zzz;"//LF &
                                   //""//LF &
                                   //"// Nested chaining."//LF &
                                   //"let x = svg.selectAll(d3.svg.something()"//LF &
                                   //"                        .zzz);"//LF &
                                   //""//LF &
                                   //"// Nothing to chain to."//LF &
                                   //"let x = svg()"//LF &
                                   //"    .zzz;"//LF &
                                   //""//LF &
                                   //"// Nothing to chain to."//LF &
                                   //"let x = svg().mumble.x() + 73"//LF &
                                   //"    .zzz;"//LF &
                                   //""//LF &
                                   //"// Local Variables:"//LF &
                                   //"// indent-tabs-mode: nil"//LF &
                                   //"// js-chain-indent: t"//LF &
                                   //"// js-indent-level: 2"//LF &
                                   //"// End:"//LF
    ! js-indent-align-list-continuation-nil.js.
    character(len=*), parameter :: R_JS_INDENT_ALIGN_LIST_CONTINUATION_NIL = &
                                   "const funcAssignment = function (arg1,"//LF &
                                   //"    arg2,"//LF &
                                   //"    arg3) {"//LF &
                                   //"    return { test: this,"//LF &
                                   //"        which: ""would"","//LF &
                                   //"        align: ""as well with the default setting"""//LF &
                                   //"    };"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"function funcDeclaration(arg1,"//LF &
                                   //"    arg2"//LF &
                                   //") {"//LF &
                                   //"    return [arg1,"//LF &
                                   //"        arg2];"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"// Local Variables:"//LF &
                                   //"// indent-tabs-mode: nil"//LF &
                                   //"// js-indent-align-list-continuation: nil"//LF &
                                   //"// End:"//LF
    ! js-indent-init-dynamic.js.
    character(len=*), parameter :: R_JS_INDENT_INIT_DYNAMIC = &
                                   "var foo = function() {"//LF &
                                   //"  return 7;"//LF &
                                   //"};"//LF &
                                   //""//LF &
                                   //"var foo = function() {"//LF &
                                   //"      return 7;"//LF &
                                   //"    },"//LF &
                                   //"    bar = 8;"//LF &
                                   //""//LF &
                                   //"var foo = function() {"//LF &
                                   //"      return 7;"//LF &
                                   //"    },"//LF &
                                   //"    bar = function() {"//LF &
                                   //"      return 8;"//LF &
                                   //"    };"//LF &
                                   //""//LF &
                                   //"// Local Variables:"//LF &
                                   //"// indent-tabs-mode: nil"//LF &
                                   //"// js-indent-level: 2"//LF &
                                   //"// js-indent-first-init: dynamic"//LF &
                                   //"// End:"//LF &
                                   //""//LF &
                                   //"// The following test intentionally produces a scan error and should"//LF &
                                   //"// be placed below all other tests to prevent awkward indentation."//LF &
                                   //"// (It still thinks it's within the body of a function.)"//LF &
                                   //""//LF &
                                   //"var foo = function() {"//LF &
                                   //"  return 7;"//LF &
                                   //"  ,"//LF &
                                   //"  bar = 8;"//LF
    ! js-indent-init-t.js.
    character(len=*), parameter :: R_JS_INDENT_INIT_T = &
                                   "var foo = function() {"//LF &
                                   //"      return 7;"//LF &
                                   //"    };"//LF &
                                   //""//LF &
                                   //"var foo = function() {"//LF &
                                   //"      return 7;"//LF &
                                   //"    },"//LF &
                                   //"    bar = 8;"//LF &
                                   //""//LF &
                                   //"var foo = function() {"//LF &
                                   //"      return 7;"//LF &
                                   //"    },"//LF &
                                   //"    bar = function() {"//LF &
                                   //"      return 8;"//LF &
                                   //"    };"//LF &
                                   //""//LF &
                                   //"// Local Variables:"//LF &
                                   //"// indent-tabs-mode: nil"//LF &
                                   //"// js-indent-level: 2"//LF &
                                   //"// js-indent-first-init: t"//LF &
                                   //"// End:"//LF
    ! js.js.
    character(len=*), parameter :: R_JS = &
                                   "var a = 1;"//LF &
                                   //"b = 2;"//LF &
                                   //""//LF &
                                   //"let c = 1,"//LF &
                                   //"    d = 2;"//LF &
                                   //""//LF &
                                   //"var e = 100500,"//LF &
                                   //"    + 1;"//LF &
                                   //""//LF &
                                   //"// Don't misinterpret ""const"""//LF &
                                   //"/const/"//LF &
                                   //""//LF &
                                   //"function test ()"//LF &
                                   //"{"//LF &
                                   //"  return /[/]/.test ('/')     // (bug#19397)"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"var f = bar('/protocols/')"//LF &
                                   //"baz();"//LF &
                                   //""//LF &
                                   //"var h = 100500"//LF &
                                   //"1;"//LF &
                                   //""//LF &
                                   //"const i = 1,"//LF &
                                   //"      j = 2;"//LF &
                                   //""//LF &
                                   //"var k = 1,"//LF &
                                   //"    l = ["//LF &
                                   //"      1, 2,"//LF &
                                   //"      3, 4"//LF &
                                   //"    ],"//LF &
                                   //"    m = 5;"//LF &
                                   //""//LF &
                                   //"var n = function() {"//LF &
                                   //"  return 7;"//LF &
                                   //"},"//LF &
                                   //"    o = 8;"//LF &
                                   //""//LF &
                                   //"foo(bar, function() {"//LF &
                                   //"  return 2;"//LF &
                                   //"});"//LF &
                                   //""//LF &
                                   //"switch (b) {"//LF &
                                   //"case ""a"":"//LF &
                                   //"  2;"//LF &
                                   //"default:"//LF &
                                   //"  3;"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"var p = {"//LF &
                                   //"  case: 'zzzz',"//LF &
                                   //"  default: 'donkey',"//LF &
                                   //"  tee: 'ornery'"//LF &
                                   //"};"//LF &
                                   //""//LF &
                                   //"var evens = [e for each (e in range(0, 21))"//LF &
                                   //"               if (ed % 2 == 0)];"//LF &
                                   //""//LF &
                                   //"var funs = ["//LF &
                                   //"  function() {"//LF &
                                   //"    for (;;) {"//LF &
                                   //"    }"//LF &
                                   //"  },"//LF &
                                   //"  function(){},"//LF &
                                   //"];"//LF &
                                   //""//LF &
                                   //"!b"//LF &
                                   //"  !=b"//LF &
                                   //"  !==b"//LF &
                                   //""//LF &
                                   //"a++"//LF &
                                   //"b +="//LF &
                                   //"  c"//LF &
                                   //""//LF &
                                   //"var re = /some value/"//LF &
                                   //"str.match(re)"//LF &
                                   //""//LF &
                                   //"baz(`http://foo.bar/${tee}`)"//LF &
                                   //"  .qux();"//LF &
                                   //""//LF &
                                   //"`multiline string"//LF &
                                   //"       contents"//LF &
                                   //"  are kept"//LF &
                                   //"        unchanged!`"//LF &
                                   //""//LF &
                                   //"class A {"//LF &
                                   //"  * x() {"//LF &
                                   //"    return 1"//LF &
                                   //"      * a(2);"//LF &
                                   //"  }"//LF &
                                   //""//LF &
                                   //"  *[Symbol.iterator]() {"//LF &
                                   //"    yield ""Foo"";"//LF &
                                   //"    yield ""Bar"";"//LF &
                                   //"  }"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"if (true)"//LF &
                                   //"  1"//LF &
                                   //"else"//LF &
                                   //"  2"//LF &
                                   //""//LF &
                                   //"Foobar"//LF &
                                   //"  .find()"//LF &
                                   //"  .catch((err) => {"//LF &
                                   //"    return 2;"//LF &
                                   //"  })"//LF &
                                   //"  .then((num) => {"//LF &
                                   //"    console.log(num);"//LF &
                                   //"  });"//LF &
                                   //""//LF &
                                   //"var z = ["//LF &
                                   //"  ...iterableObj,"//LF &
                                   //"  4,"//LF &
                                   //"  5"//LF &
                                   //"]"//LF &
                                   //""//LF &
                                   //"var arr = ["//LF &
                                   //"  -1, 2,"//LF &
                                   //"  -3, 4 +"//LF &
                                   //"    -5"//LF &
                                   //"];"//LF &
                                   //""//LF &
                                   //"// Regression test for bug#15582."//LF &
                                   //"if (x > 72 &&"//LF &
                                   //"    y < 85) { // found"//LF &
                                   //"  do_something();"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"// Test that chaining doesn't happen when js-chain-indent is nil."//LF &
                                   //"let x = svg.mumble()"//LF &
                                   //"    .zzz;"//LF &
                                   //""//LF &
                                   //"// https://github.com/mooz/js2-mode/issues/405"//LF &
                                   //"if (1) {"//LF &
                                   //"  isSet"//LF &
                                   //"    ? (isEmpty ? 2 : 3)"//LF &
                                   //"    : 4"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"// Regexp is not a continuation"//LF &
                                   //"bar("//LF &
                                   //"  ""string arg1"","//LF &
                                   //"  /abc/"//LF &
                                   //")"//LF &
                                   //""//LF &
                                   //"// No infloop inside js--re-search-backward-inner"//LF &
                                   //"let b = {"//LF &
                                   //"  a : `"//LF &
                                   //"  //1"//LF &
                                   //"  `"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"// bug#25904"//LF &
                                   //"foo.bar.baz(very => // A comment"//LF &
                                   //"  very"//LF &
                                   //").biz(([baz={a: [123]}, boz]) =>"//LF &
                                   //"  baz"//LF &
                                   //").snarf((snorf) => /* Another comment */"//LF &
                                   //"  snorf"//LF &
                                   //");"//LF &
                                   //""//LF &
                                   //"// Continuation of bug#25904; support broken arrow as N+1th arg"//LF &
                                   //"map(arr, (val) =>"//LF &
                                   //"  val"//LF &
                                   //")"//LF &
                                   //""//LF &
                                   //"// Local Variables:"//LF &
                                   //"// indent-tabs-mode: nil"//LF &
                                   //"// js-indent-level: 2"//LF &
                                   //"// End:"//LF

contains

    !> Collect all exported unit tests.
    subroutine collect_js_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [new_unittest("js-mode-regexp-syntax", test_regexp_syntax), &
                     new_unittest("js-mode-regexp-syntax-bug-25529", test_regexp_syntax_bug_25529), &
                     new_unittest("js-mode-indentation-error", test_indentation_error), &
                     new_unittest("js-mode-doc-comment-face", test_doc_comment_face), &
                     new_unittest("js-mode-propertize-bug-1", test_propertize_bug_1), &
                     new_unittest("js-mode-propertize-bug-2", test_propertize_bug_2), &
                     new_unittest("js-indent-test/js-chain.js", test_indent_chain), &
                     new_unittest("js-indent-test/js-indent-align-list-continuation-nil.js", test_indent_align_nil), &
                     new_unittest("js-indent-test/js-indent-init-dynamic.js", test_indent_init_dynamic), &
                     new_unittest("js-indent-test/js-indent-init-t.js", test_indent_init_t), &
                     new_unittest("js-indent-test/js.js", test_indent_js), &
                     new_unittest("js-mode-beginning-of-defun", test_beginning_of_defun), &
                     new_unittest("js-mode-end-of-defun", test_end_of_defun), &
                     new_unittest("js-syntactic-context", test_syntactic_context), &
                     new_unittest("js-font-lock-function-name", test_font_lock_function_name)]
    end subroutine collect_js_fm

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    ! A temporary buffer holding TEXT (in JavaScript mode if JS, before
    ! TEXT is inserted when FIRST).
    subroutine temp_buffer(text, first)
        character(len=*), intent(in) :: text
        logical, intent(in) :: first
        type(handle_type) :: buf
        type(status_type) :: st

        call init_bindings()
        call get_buffer_create(" *js-test*", buf, st)
        call kill_buffer(buf, st)
        call get_buffer_create(" *js-test*", buf, st)
        call set_buffer(buf, st)
        call widen()
        call erase_buffer(st)
        if (first) call js_mode(st)
        call insert(text, st)
        if (.not. first) call js_mode(st)
    end subroutine temp_buffer

    ! INSERT text at point.
    subroutine ins(text)
        character(len=*), intent(in) :: text
        type(status_type) :: st

        call insert(text, st)
    end subroutine ins

    ! The text of the current buffer.
    function buffer_string() result(s)
        character(len=:), allocatable :: s
        type(status_type) :: st

        call buffer_substring(point_min(), point_max(), s, st)
        if (.not. allocated(s)) s = ""
    end function buffer_string

    ! The face at point (the first of a list), "" if none.
    function face() result(name)
        character(len=:), allocatable :: name
        type(value_type) :: v
        type(value_type), allocatable :: elts(:)
        type(status_type) :: st

        name = ""
        call get_text_property(point(), sym("face"), v, st)

        if (consp(v)) then
            call list_elements(v, elts)
            v = elts(1)
        end if

        if (nilp(v)) return
        if (symbolp(v)) name = symbols(xsymbol(v))%name
    end function face

    ! Fontify the current buffer.
    subroutine fontify()
        type(status_type) :: st

        call minor_modes%font_lock_mode%enable(st)
        call font_lock_ensure(st)
    end subroutine fontify

    ! Remove all indentation in the current buffer
    ! (js-tests--remove-indentation).
    subroutine remove_indentation()
        type(parse_state_type) :: state
        type(status_type) :: st
        integer(int64) :: pos
        integer(int64), allocatable :: saved(:)

        call goto_char(point_min())

        do
            call re_search_forward("^[ "//TAB//"]+", pos, st, noerror=SEARCH_NOERROR)
            if (pos == 0) exit
            saved = match_data()
            call syntax_ppss(state)
            call set_match_data(saved)
            call goto_char(pos)
            ! Avoid multiline string literals.
            if (state%instring < 0) call replace_match("", st)
        end do
    end subroutine remove_indentation

    ! js-deftest-indent: reindenting TEXT gives TEXT back, twice.
    subroutine check_indent(error, text)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: text
        type(status_type) :: st

        call remove_indentation()
        ! Indent and check that we get the original text.
        call indent_region(point_min(), point_max(), st)
        call check(error, buffer_string() == text, "reindenting changed the text: "//LF//buffer_string())
        if (allocated(error)) return
        ! Verify idempotency.
        call indent_region(point_min(), point_max(), st)
        call check(error, buffer_string() == text, "reindenting twice changed the text")
    end subroutine check_indent

    ! The file-local variables of the resource files.
    subroutine set_locals(level, chain, align, first_init)
        integer, intent(in) :: level
        logical, intent(in) :: chain, align
        integer, intent(in) :: first_init

        call indent_tabs_mode%set_local(.false.)
        call js_indent_level%set_local(level)
        call js_chain_indent%set_local(chain)
        call js_indent_align_list_continuation%set_local(align)
        call js_indent_first_init%set_local(first_init)
    end subroutine set_locals

    ! ------------------------------------------------------------------
    ! Tests
    ! ------------------------------------------------------------------

    subroutine test_regexp_syntax(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: COLUMNS(11) = [0, 4, 8, 4, 4, 8, 4, 4, 8, 4, 0]
        type(status_type) :: st
        integer(int64) :: shortage
        integer :: i

        ! Normally indentation tests are done in manual/indent, but in this
        ! case we are specifically testing a case where the bug caused the
        ! indenter not to do anything, and manual/indent can only be used
        ! for already-correct files.
        call temp_buffer("function f(start, value) {"//LF//"if (start - 1 === 0 || /[ (:,='""]/.test(value)) {"//LF &
                         //"--start;"//LF//"}"//LF//"if (start - 1 === 0 && /[ (:,='""]/.test(value)) {"//LF &
                         //"--start;"//LF//"}"//LF//"if (!/[ (:,='""]/.test(value)) {"//LF//"--start;"//LF//"}"//LF &
                         //"}"//LF, .false.)
        call indent_region(point_min(), point_max(), st)
        call goto_char(point_min())

        do i = 1, size(COLUMNS)
            call back_to_indentation()
            call check(error, int(current_column()), COLUMNS(i))
            if (allocated(error)) return
            shortage = forward_line(1_int64)
        end do
    end subroutine test_regexp_syntax

    subroutine test_regexp_syntax_bug_25529(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=16), parameter :: CONTENTS(3) = [character(len=16) :: "[^[]", "[/]", "*comment*/"//LF//"/regexp"]
        type(parse_state_type) :: state
        integer(int64) :: here
        integer :: i

        do i = 1, 3
            call temp_buffer("", .true.)
            call ins("let x = /"//trim(CONTENTS(i))//"/;"//LF)
            here = point()
            call ins("something();"//LF)
            call goto_char(here)
            ! The failure mode was that the regexp literal was not
            ! recognized, causing the next line to be given string syntax;
            ! but check for comment syntax as well to prevent an
            ! implementation not recognizing the comment example.
            call syntax_ppss(state)
            call check(error, state%instring < 0 .and. state%incomment == 0, trim(CONTENTS(i)))
            if (allocated(error)) return
        end do
    end subroutine test_regexp_syntax_bug_25529

    subroutine test_indentation_error(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("", .true.)
        ! The bug previously was that requesting re-indentation on the "{"
        ! line here threw an exception.
        call ins("const TESTS = ["//LF//"{")
        call js_indent_line(st)
        ! Any success is ok here.
        call check(error, .true.)
    end subroutine test_indentation_error

    subroutine test_doc_comment_face(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=3), parameter :: OPENERS(4) = [character(len=3) :: "/*", "//", "/**", """"]
        character(len=2), parameter :: CLOSERS(4) = [character(len=2) :: "*/", LF, "*/", """"]
        character(len=22), parameter :: FACES(4) = [character(len=22) :: "font_lock_comment_face", &
                                                            "font_lock_comment_face", "font_lock_doc_face", "font_lock_string_face"]
        integer(int64) :: here
        integer :: i

        do i = 1, 4
            call temp_buffer("", .true.)
            call ins(trim(OPENERS(i))//" he")
            here = point()
            call ins("llo "//trim(CLOSERS(i)))
            call goto_char(here)
            call fontify()
            call goto_char(here)
            call check(error, face(), trim(FACES(i)))
            if (allocated(error)) return
        end do
    end subroutine test_doc_comment_face

    subroutine test_propertize_bug_1(error)
        type(error_type), allocatable, intent(out) :: error

        call temp_buffer("", .true.)
        call ins("x")
        call goto_char(point_min())
        call ins("/")
        ! The bug was a hang.
        call check(error, .true.)
    end subroutine test_propertize_bug_1

    subroutine test_propertize_bug_2(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: ZEROS = "        00000000000000000000000000000000000000000000000000;"//LF
        integer(int64) :: here

        call temp_buffer("", .true.)
        call ins("function f() {"//LF//"    function g()"//LF//"    {"//LF//"        1 / 2;"//LF//"    }"//LF//LF &
                 //"    function h() {"//LF)
        here = point()
        call ins(LF//repeat(ZEROS, 8)//"        00;"//LF//"    }"//LF//"}"//LF)
        call goto_char(here)
        call ins("/")
        ! The bug was a hang.
        call check(error, .true.)
    end subroutine test_propertize_bug_2

    subroutine test_indent_chain(error)
        type(error_type), allocatable, intent(out) :: error

        call temp_buffer(R_JS_CHAIN, .false.)
        call set_locals(2, .true., .true., JS_INDENT_FIRST_INIT_NIL)
        call check_indent(error, R_JS_CHAIN)
    end subroutine test_indent_chain

    subroutine test_indent_align_nil(error)
        type(error_type), allocatable, intent(out) :: error

        call temp_buffer(R_JS_INDENT_ALIGN_LIST_CONTINUATION_NIL, .false.)
        call set_locals(4, .false., .false., JS_INDENT_FIRST_INIT_NIL)
        call check_indent(error, R_JS_INDENT_ALIGN_LIST_CONTINUATION_NIL)
    end subroutine test_indent_align_nil

    subroutine test_indent_init_dynamic(error)
        type(error_type), allocatable, intent(out) :: error

        call temp_buffer(R_JS_INDENT_INIT_DYNAMIC, .false.)
        call set_locals(2, .false., .true., JS_INDENT_FIRST_INIT_DYNAMIC)
        call check_indent(error, R_JS_INDENT_INIT_DYNAMIC)
    end subroutine test_indent_init_dynamic

    subroutine test_indent_init_t(error)
        type(error_type), allocatable, intent(out) :: error

        call temp_buffer(R_JS_INDENT_INIT_T, .false.)
        call set_locals(2, .false., .true., JS_INDENT_FIRST_INIT_T)
        call check_indent(error, R_JS_INDENT_INIT_T)
    end subroutine test_indent_init_t

    subroutine test_indent_js(error)
        type(error_type), allocatable, intent(out) :: error

        call temp_buffer(R_JS, .false.)
        call set_locals(2, .false., .true., JS_INDENT_FIRST_INIT_NIL)
        call check_indent(error, R_JS)
    end subroutine test_indent_js

    ! The buffer of the defun tests.
    subroutine defun_buffer()
        call temp_buffer("function foo() {"//LF//"  var value = 1;"//LF//"}"//LF//LF//"/** A comment. */"//LF &
                         //"function bar() {"//LF//"  var value = 1;"//LF//"}"//LF, .false.)
    end subroutine defun_buffer

    subroutine test_beginning_of_defun(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        logical :: found

        call defun_buffer()
        ! Move point inside `foo'.
        call goto_char(18_int64)
        call beginning_of_defun(1_int64, found, st)
        call check(error, bobp(), "inside foo")
        if (allocated(error)) return
        ! Move point between the two functions.
        call goto_char(37_int64)
        call beginning_of_defun(1_int64, found, st)
        call check(error, bobp(), "between the functions")
        if (allocated(error)) return
        ! Move point inside `bar'.
        call goto_char(73_int64)
        call beginning_of_defun(1_int64, found, st)
        ! Point should move to the beginning of `bar'.
        call check(error, point(), 56_int64)
    end subroutine test_beginning_of_defun

    subroutine test_end_of_defun(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call defun_buffer()
        call goto_char(point_min())
        call end_of_defun(1_int64, st)
        ! end-of-defun from the beginning of the buffer should go to the
        ! end of `foo'.
        call check(error, point(), 37_int64)
        if (allocated(error)) return
        ! Move point to the beginning of /** A comment. */
        call goto_char(38_int64)
        call end_of_defun(1_int64, st)
        ! end-of-defun should move point to eob.
        call check(error, eobp(), "not at eob")
    end subroutine test_end_of_defun

    ! Own: the syntactic context inside and outside a function.
    subroutine test_syntactic_context(error)
        type(error_type), allocatable, intent(out) :: error

        call defun_buffer()
        call goto_char(20_int64)
        call check(error, js_syntactic_context(), JS_CONTEXT_FUNCTION)
        if (allocated(error)) return
        call goto_char(37_int64)
        call check(error, js_syntactic_context(), JS_CONTEXT_TOPLEVEL)
    end subroutine test_syntactic_context

    ! Own: the function name, a keyword and a variable get their faces.
    subroutine test_font_lock_function_name(error)
        type(error_type), allocatable, intent(out) :: error

        call temp_buffer("function foo(a, b) {"//LF//"  var value = 1;"//LF//"}"//LF, .false.)
        call fontify()
        call goto_char(10_int64)
        call check(error, face(), "font_lock_function_name_face")
        if (allocated(error)) return
        call goto_char(14_int64)
        call check(error, face(), "font_lock_variable_name_face")
        if (allocated(error)) return
        call goto_char(24_int64)
        call check(error, face(), "font_lock_keyword_face")
        if (allocated(error)) return
        call goto_char(28_int64)
        call check(error, face(), "font_lock_variable_name_face")
    end subroutine test_font_lock_function_name
end module test_js_fm
