! test_css_mode_fm.f90 -- tests for CSS mode (css_mode_fm).
! Ports GNU Emacs 31.1 test/lisp/textmodes/css-mode-tests.el.
!
! Copyright (C) 2016-2026 Free Software Foundation, Inc.
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

!> Ports test/lisp/textmodes/css-mode-tests.el (the resource files are
!> embedded; the selector tests combine the selectors in a fixed order
!> instead of a random one), with own tests of color fontification and
!> filling.
module test_css_mode_fm
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use buffer_fm, only: get_buffer_create, set_buffer, erase_buffer, kill_buffer
    use insdel_fm, only: insert
    use lisp_fm, only: value_type, nilp, consp, symbolp, xsymbol, list_elements, stringp, xstring, symbols
    use lread_fm, only: sym
    use editfns_fm, only: point, point_min, point_max, goto_char, widen, buffer_substring, line_end_position
    use simple_fm, only: backward_word
    use indent_fm, only: indent_region
    use fill_fm, only: fill_paragraph
    use textprop_fm, only: get_text_property
    use font_lock_fm, only: font_lock_ensure
    use modes_fm, only: minor_modes
    use minibuf_table_fm, only: capf_data_type
    use minibuf_fm, only: all_completions
    use bindings_fm, only: init_bindings
    use tty_colors_fm, only: tty_register_default_colors, tty_color_clear
    use css_mode_fm, only: css_mode, scss_mode, css_property_values, css_value_class_lookup, &
                           css_property_value_cache_get, css_current_rule_name, css_completion_at_point, &
                           css_foreign_completions, css_class_list_function, css_id_list_function, css_color_to_4_dpc, &
                           css_format_hex, css_named_color_to_hex, css_format_rgba_alpha, css_hex_to_rgb, &
                           css_rgb_to_named_color_or_hex, css_cycle_color_format, css_join_nested_selectors, &
                           css_mdn_find_symbol, css_rgb_color, css_hsl_color, css_hex_color, css_hex_alpha, css_named_color
    implicit none(type, external)
    private

    public :: collect_css_mode_fm

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

    ! test-indent.css.
    character(len=*), parameter :: TEST_INDENT_CSS = &
                                   "/* asdfasdf */"//LF &
                                   //""//LF &
                                   //".xxx"//LF &
                                   //"{"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"article[role=""main""] {"//LF &
                                   //"    width: 60%;"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"a, b:hover, c {"//LF &
                                   //"    color: black !important;"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"a, b:hover {                    /* bug:20282 */"//LF &
                                   //"    c {"//LF &
                                   //"        color: black;"//LF &
                                   //"    }"//LF &
                                   //"    color: black;"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"a.b:c,d.e:f,g[h]:i,j[k]:l,.m.n:o,.p.q:r,.s[t]:u,.v[w]:x { /* bug:20282 */"//LF &
                                   //"    background-color: white;"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"/* asdfasdf */"//LF &
                                   //"@foo x2 {"//LF &
                                   //"    bla:toto;"//LF &
                                   //"}"//LF &
                                   //".x2"//LF &
                                   //"{"//LF &
                                   //"    /* foo: bar; */ foo2: bar2;"//LF &
                                   //"    bar1: url(""http://toto/titi"");"//LF &
                                   //"    bar2: url('http://toto/titi');"//LF &
                                   //"    bar3: url(http://toto/titi);"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"div.x3"//LF &
                                   //"{"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"article:hover"//LF &
                                   //"{"//LF &
                                   //"    color: black;"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"/* bug:13425 */"//LF &
                                   //"div:first-child,"//LF &
                                   //"div:last-child,"//LF &
                                   //"div[disabled],"//LF &
                                   //"div::before {"//LF &
                                   //"    font: 15px ""Helvetica Neue"","//LF &
                                   //"          Helvetica,"//LF &
                                   //"          Arial,"//LF &
                                   //"          ""Nimbus Sans L"","//LF &
                                   //"          sans-serif;"//LF &
                                   //"    font: 15px ""Helvetica Neue"", Helvetica, Arial,"//LF &
                                   //"          ""Nimbus Sans L"", sans-serif;"//LF &
                                   //"    background: no-repeat right"//LF &
                                   //"                5px center;"//LF &
                                   //"    transform: matrix(1.0, 2.0,"//LF &
                                   //"                      3.0, 4.0,"//LF &
                                   //"                      5.0, 6.0);"//LF &
                                   //"    transform: matrix("//LF &
                                   //"        1.0, 2.0,"//LF &
                                   //"        3.0, 4.0,"//LF &
                                   //"        5.0, 6.0"//LF &
                                   //"    );"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"/* Multi-line selector including both a pseudo-class and"//LF &
                                   //"   parenthesis. */"//LF &
                                   //".form-group:not(.required) label,"//LF &
                                   //".birth-date .row > * {"//LF &
                                   //"    &::after {"//LF &
                                   //"        display: inline;"//LF &
                                   //"        font-weight: normal;"//LF &
                                   //"    }"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"@font-face {"//LF &
                                   //"    src: url(""Sans-Regular.eot"") format(""eot""),"//LF &
                                   //"         url(""Sans-Regular.woff"") format(""woff""),"//LF &
                                   //"         url(""Sans-Regular.ttf"") format(""truetype"");"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //"@font-face {"//LF &
                                   //"    src:"//LF &
                                   //"        url(""Sans-Regular.eot"") format(""eot""),"//LF &
                                   //"        url(""Sans-Regular.woff"") format(""woff"");"//LF &
                                   //"}"//LF &
                                   //""//LF &
                                   //".foo-bar--baz {"//LF &
                                   //"    --foo-variable: 5px;"//LF &
                                   //"    --_variable_with_underscores: #fff;"//LF &
                                   //"    --_variable-starting-with-underscore: none;"//LF &
                                   //"    margin: var(--foo-variable);"//LF &
                                   //"    color: var(--_variable_with_underscores);"//LF &
                                   //"    display: var(--_variable-starting-with-underscore);"//LF &
                                   //"}"//LF
    character(len=*), parameter :: CSS_SELECTORS(*) = [character(len=48) :: &
                                                       "#firstname", &
                                                       "*", &
                                                       "p", &
                                                       "p.intro", &
                                                       "div, p", &
                                                       "div p", &
                                                       "div > p", &
                                                       "div + p", &
                                                       "p ~ ul", &
                                                       "[target]", &
                                                       "[target=_blank]", &
                                                       "[title~=flower]", &
                                                       "[lang|=en]", &
                                                       "a[href^=""https""]", &
                                                       "a[href$="".pdf""]", &
                                                       "a[href*=""w3schools""]", &
                                                       "a:active", &
                                                       "p::after", &
                                                       "p::before", &
                                                       "input:checked", &
                                                       "input:default", &
                                                       "input:disabled", &
                                                       "p:empty", &
                                                       "input:enabled", &
                                                       "p:first-child", &
                                                       "p::first-letter", &
                                                       "p::first-line", &
                                                       "p:first-of-type", &
                                                       "input:focus", &
                                                       ":fullscreen", &
                                                       "a:hover", &
                                                       "input:in-range", &
                                                       "input:indeterminate", &
                                                       "input:invalid", &
                                                       "p:lang(it)", &
                                                       "p:last-child", &
                                                       "p:last-of-type", &
                                                       "a:link", &
                                                       "::marker", &
                                                       ":not(p)", &
                                                       "p:nth-child(2)", &
                                                       "p:nth-last-child(2)", &
                                                       "p:nth-last-of-type(2)", &
                                                       "p:nth-of-type(2)", &
                                                       "p:only-of-type", &
                                                       "p:only-child", &
                                                       "input:optional", &
                                                       "input:out-of-range", &
                                                       "input:read-only", &
                                                       "input:read-write", &
                                                       "input:required", &
                                                       ":root", &
                                                       "::selection", &
                                                       "#news:target", &
                                                       "input:valid", &
                                                       "a:visited"]
    character(len=*), parameter :: SCSS_SELECTORS(*) = [character(len=48) :: &
                                                        "p.#{$name} var", &
                                                        "p.#{$name}:active var", &
                                                        "p.#{$name}::after var", &
                                                        "f.#{$bar}::after p::after", &
                                                        "p.#{$name} f.#{$bar} k.var #{$bar}  #{$bar}", &
                                                        "p.#{$name}", &
                                                        "&:hover", &
                                                        "> li", &
                                                        "+ li", &
                                                        "~ li"]

contains

    !> Collect all exported unit tests.
    !> Collect all exported unit tests.
    subroutine collect_css_mode_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [new_unittest("css-test-property-values", test_property_values), &
                     new_unittest("css-test-property-value-cache", test_property_value_cache), &
                     new_unittest("css-test-property-values-no-duplicates", test_property_values_no_duplicates), &
                     new_unittest("css-test-value-class-lookup", test_value_class_lookup), &
                     new_unittest("css-test-current-defun-name", test_current_subprogram_name), &
                     new_unittest("css-test-current-defun-name-nested", test_current_subprogram_name_nested), &
                     new_unittest("css-test-current-defun-name-complex", test_current_subprogram_name_complex), &
                     new_unittest("css-test-complete-bang-rule", test_complete_bang_rule), &
                     new_unittest("scss-test-complete-bang-rule", test_scss_complete_bang_rule), &
                     new_unittest("css-test-complete-property-value", test_complete_property_value), &
                     new_unittest("css-test-complete-pseudo-class", test_complete_pseudo_class), &
                     new_unittest("css-test-complete-pseudo-element", test_complete_pseudo_element), &
                     new_unittest("css-test-complete-at-rule", test_complete_at_rule), &
                     new_unittest("scss-test-complete-at-rule", test_scss_complete_at_rule), &
                     new_unittest("css-test-complete-property", test_complete_property), &
                     new_unittest("css-test-foreign-completions", test_foreign_completions), &
                     new_unittest("css-test-complete-selector-tag", test_complete_selector_tag), &
                     new_unittest("css-test-complete-selector-class", test_complete_selector_class), &
                     new_unittest("css-test-complete-selector-id", test_complete_selector_id), &
                     new_unittest("css-test-complete-nested-selector", test_complete_nested_selector), &
                     new_unittest("scss-test-complete-nested-selector", test_scss_complete_nested_selector), &
                     new_unittest("css-test-color-to-4-dpc", test_color_to_4_dpc), &
                     new_unittest("css-test-format-hex", test_format_hex), &
                     new_unittest("css-test-named-color-to-hex", test_named_color_to_hex), &
                     new_unittest("css-test-format-rgba-alpha", test_format_rgba_alpha), &
                     new_unittest("css-test-hex-to-rgb", test_hex_to_rgb), &
                     new_unittest("css-test-rgb-to-named-color-or-hex", test_rgb_to_named_color_or_hex), &
                     new_unittest("css-test-cycle-color-format", test_cycle_color_format), &
                     new_unittest("css-test-join-nested-selectors", test_join_nested_selectors), &
                     new_unittest("css-mdn-symbol-guessing", test_mdn_symbol_guessing), &
                     new_unittest("css-test-rgb-parser", test_rgb_parser), &
                     new_unittest("css-test-hsl-parser", test_hsl_parser), &
                     new_unittest("css-test-hex-color", test_hex_color), &
                     new_unittest("css-test-hex-alpha", test_hex_alpha), &
                     new_unittest("css-test-named-color", test_named_color), &
                     new_unittest("css-mode-test-indent", test_indent), &
                     new_unittest("css-mode-test-selectors", test_selectors), &
                     new_unittest("scss-mode-test-selectors", test_scss_selectors), &
                     new_unittest("css-fontify-colors", test_fontify_colors), &
                     new_unittest("css-fill-block", test_fill_block), &
                     new_unittest("css-fill-comment", test_fill_comment)]
    end subroutine collect_css_mode_fm

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    ! A temporary buffer, in CSS mode (SCSS mode if SCSS; Fundamental mode
    ! if PLAIN).
    subroutine temp_buffer(scss, plain)
        logical, intent(in), optional :: scss, plain
        type(handle_type) :: buf
        type(status_type) :: st

        call init_bindings()
        call get_buffer_create(" *css-test*", buf, st)
        call set_buffer(buf, st)
        call kill_buffer(buf, st)
        call get_buffer_create(" *css-test*", buf, st)
        call set_buffer(buf, st)
        call widen()
        call erase_buffer(st)

        if (present(plain)) then
            if (plain) return
        end if

        if (present(scss)) then
            if (scss) then
                call scss_mode(st)
                return
            end if
        end if

        call css_mode(st)
    end subroutine temp_buffer

    ! The text of the current buffer.
    function buffer_string() result(s)
        character(len=:), allocatable :: s

        s = text_between(point_min(), point_max())
    end function buffer_string

    ! The text from START to END.
    function text_between(start, end) result(text)
        integer(int64), intent(in) :: start, end
        character(len=:), allocatable :: text
        type(status_type) :: st

        call buffer_substring(start, end, text, st)
        if (.not. allocated(text)) text = ""
    end function text_between

    ! INSERT text at point.
    subroutine ins(text)
        character(len=*), intent(in) :: text
        type(status_type) :: st

        call insert(text, st)
    end subroutine ins

    ! Whether the sorted array A is the same as B.
    logical function same_sorted(a, b)
        character(len=*), intent(in) :: a(:), b(:)
        character(len=64), allocatable :: x(:), y(:)

        same_sorted = .false.
        if (size(a) /= size(b)) return
        x = sorted(a)
        y = sorted(b)
        same_sorted = all(x == y)
    end function same_sorted

    ! A sorted, of length 64.
    function sorted(a) result(s)
        character(len=*), intent(in) :: a(:)
        character(len=64), allocatable :: s(:)
        character(len=64) :: t
        integer :: i, j

        allocate (s(size(a)))
        s = a

        do i = 2, size(s)
            t = s(i)
            j = i - 1

            do while (j >= 1)
                if (llt(t, s(j))) then
                    s(j + 1) = s(j)
                    j = j - 1
                else
                    exit
                end if
            end do

            s(j + 1) = t
        end do
    end function sorted

    ! The completions at point (css-mode-tests--completions).
    function completions() result(names)
        character(len=64), allocatable :: names(:)
        type(capf_data_type) :: data
        type(status_type) :: st
        type(value_type) :: v
        type(value_type), allocatable :: elts(:)
        integer :: i

        allocate (names(0))
        call css_completion_at_point(data, st)
        if (.not. data%found) return
        call all_completions(text_between(data%start, data%end), data%table, v, st)
        call list_elements(v, elts)
        deallocate (names)
        allocate (names(size(elts)))

        do i = 1, size(elts)
            names(i) = xstring(elts(i))
        end do
    end function completions

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

    ! Fontify the current buffer (font-lock-debug-fontify).
    subroutine fontify()
        type(status_type) :: st

        call minor_modes%font_lock_mode%enable(st)
        call font_lock_ensure(st)
    end subroutine fontify

    ! The subprogram name at point, "" for none.
    function subprogram_name() result(name)
        character(len=:), allocatable :: name
        logical :: found

        call css_current_rule_name(name, found)
        if (.not. found) name = "<nil>"
    end function subprogram_name

    ! The completion list functions of the tests.
    subroutine foo_bar(names)
        character(len=:), allocatable, intent(out) :: names(:)

        names = [character(len=3) :: "foo", "bar"]
    end subroutine foo_bar

    subroutine bar_baz(names)
        character(len=:), allocatable, intent(out) :: names(:)

        names = [character(len=3) :: "bar", "baz"]
    end subroutine bar_baz

    ! ------------------------------------------------------------------
    ! Tests
    ! ------------------------------------------------------------------

    subroutine test_property_values(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=36), allocatable :: a(:)

        call init_bindings()
        ! The `float' property has a flat value list.
        call check(error, same_sorted(css_property_values("float"), [character(len=5) :: "left", "none", "right"]))
        if (allocated(error)) return
        ! The `list-style' property refers to several other properties.
        a = css_property_values("list-style-type")
        a = merge_new(a, css_property_values("list-style-position"))
        a = merge_new(a, css_property_values("list-style-image"))
        call check(error, same_sorted(css_property_values("list-style"), a))
        if (allocated(error)) return
        ! The `position' property is tricky because it's also the name of a
        ! value class.
        call check(error, same_sorted(css_property_values("position"), &
                                      [character(len=8) :: "absolute", "fixed", "relative", "static"]))
        if (allocated(error)) return
        ! The `background-position' property should refer to the `position'
        ! value class, not the property of the same name.
        a = css_value_class_lookup("position")
        call check(error, all(css_property_values("background-position") == a))
        if (allocated(error)) return
        ! Check that the `color' property doesn't cause infinite recursion
        ! because it refers to the value class of the same name.
        call check(error, size(css_property_values("color")), 154)
    end subroutine test_property_values

    ! A with the elements of B it doesn't have (seq-uniq of append).
    function merge_new(a, b) result(c)
        character(len=36), intent(in) :: a(:), b(:)
        character(len=36), allocatable :: c(:)
        integer :: i

        c = a

        do i = 1, size(b)
            if (.not. any(c == b(i))) c = [character(len=36) :: c, b(i)]
        end do
    end function merge_new

    !> Test that `css--property-value-cache' is in use.
    subroutine test_property_value_cache(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=36), allocatable :: values(:), cached(:)
        logical :: found

        call init_bindings()
        call css_property_value_cache_get("word-wrap", cached, found)
        call check(error,.not. found)
        if (allocated(error)) return
        values = css_property_values("word-wrap")
        call css_property_value_cache_get("word-wrap", cached, found)
        call check(error, found)
        if (allocated(error)) return
        call check(error, all(cached == values))
    end subroutine test_property_value_cache

    !> Test that `css--property-values' returns no duplicates.
    subroutine test_property_values_no_duplicates(error)
        type(error_type), allocatable, intent(out) :: error

        call init_bindings()
        ! The `flex' property is prone to duplicate values; if they aren't
        ! removed, it'll contain at least two instances of `auto'.
        call check(error, same_sorted(css_property_values("flex"), &
                                      [character(len=7) :: "auto", "calc()", "content", "none"]))
    end subroutine test_property_values_no_duplicates

    subroutine test_value_class_lookup(error)
        type(error_type), allocatable, intent(out) :: error

        call init_bindings()
        call check(error, same_sorted(css_value_class_lookup("position"), &
                                      [character(len=6) :: "bottom", "calc()", "center", "left", "right", "top"]))
    end subroutine test_value_class_lookup

    subroutine test_current_subprogram_name(error)
        type(error_type), allocatable, intent(out) :: error

        call temp_buffer(plain=.true.)
        call ins("body { top: 0; }")
        call goto_char(7_int64)
        call check(error, subprogram_name(), "body")
        if (allocated(error)) return
        call goto_char(15_int64)
        call check(error, subprogram_name(), "body")
    end subroutine test_current_subprogram_name

    subroutine test_current_subprogram_name_nested(error)
        type(error_type), allocatable, intent(out) :: error

        call temp_buffer(plain=.true.)
        call ins("body > .main a { top: 0; }")
        call goto_char(20_int64)
        call check(error, subprogram_name(), "body > .main a")
    end subroutine test_current_subprogram_name_nested

    subroutine test_current_subprogram_name_complex(error)
        type(error_type), allocatable, intent(out) :: error

        call temp_buffer(plain=.true.)
        call ins("input[type=submit]:hover { color: red; }")
        call goto_char(30_int64)
        call check(error, subprogram_name(), "input[type=submit]:hover")
    end subroutine test_current_subprogram_name_complex

    subroutine test_complete_bang_rule(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=64), allocatable :: c(:)

        call temp_buffer()
        call ins("body { left: 0 !")
        c = completions()
        call check(error, any(c == "important"))
        if (allocated(error)) return
        ! Don't include SCSS bang-rules
        call check(error,.not. any(c == "default"))
    end subroutine test_complete_bang_rule

    subroutine test_scss_complete_bang_rule(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=64), allocatable :: c(:)

        call temp_buffer(scss=.true.)
        call ins("body { left: 0 !")
        c = completions()
        call check(error, any(c == "important"))
        if (allocated(error)) return
        call check(error, any(c == "default"))
    end subroutine test_scss_complete_bang_rule

    subroutine test_complete_property_value(error)
        type(error_type), allocatable, intent(out) :: error

        call temp_buffer()
        call ins("body { position: ")
        call check(error, same_sorted(completions(), [character(len=8) :: "absolute", "fixed", "inherit", "initial", &
                                                      "relative", "static", "unset"]))
    end subroutine test_complete_property_value

    subroutine test_complete_pseudo_class(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=64), allocatable :: c(:)

        call temp_buffer()
        call ins("body:a")
        c = completions()
        call check(error, any(c == ":active"))
        if (allocated(error)) return
        call check(error,.not. any(c == ":disabled"))
        if (allocated(error)) return
        ! Don't include pseudo-elements
        call check(error,.not. any(c == "::after"))
    end subroutine test_complete_pseudo_class

    subroutine test_complete_pseudo_element(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=64), allocatable :: c(:)

        call temp_buffer()
        call ins("body::a")
        c = completions()
        call check(error, any(c == "::after"))
        if (allocated(error)) return
        call check(error,.not. any(c == "::disabled"))
        if (allocated(error)) return
        ! Don't include pseudo-classes
        call check(error,.not. any(c == ":active"))
    end subroutine test_complete_pseudo_element

    subroutine test_complete_at_rule(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=64), allocatable :: c(:)

        call temp_buffer()
        call ins("@m")
        c = completions()
        call check(error, any(c == "media"))
        if (allocated(error)) return
        call check(error,.not. any(c == "keyframes"))
        if (allocated(error)) return
        ! Don't include SCSS at-rules
        call check(error,.not. any(c == "mixin"))
    end subroutine test_complete_at_rule

    subroutine test_scss_complete_at_rule(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=64), allocatable :: c(:)

        call temp_buffer(scss=.true.)
        call ins("@m")
        c = completions()
        call check(error, any(c == "media"))
        if (allocated(error)) return
        call check(error,.not. any(c == "keyframes"))
        if (allocated(error)) return
        call check(error, any(c == "mixin"))
    end subroutine test_scss_complete_at_rule

    subroutine test_complete_property(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=64), allocatable :: c(:)

        call temp_buffer()
        call ins("body { f")
        c = completions()
        call check(error, any(c == "filter"))
        if (allocated(error)) return
        call check(error,.not. any(c == "position"))
        if (allocated(error)) return
        ! Bug#27392
        call temp_buffer()
        call ins("html { grid")
        c = completions()
        call check(error, size(c) > 0)
    end subroutine test_complete_property

    subroutine test_foreign_completions(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: b1, b2, saved
        type(status_type) :: st
        character(len=:), allocatable :: c(:)

        call temp_buffer(plain=.true.)
        call get_buffer_create(" *css-test*", saved, st)
        call get_buffer_create(" *css-1*", b1, st)
        call get_buffer_create(" *css-2*", b2, st)
        call set_buffer(b1, st)
        call css_class_list_function%set_local(foo_bar)
        call set_buffer(b2, st)
        call css_class_list_function%set_local(bar_baz)
        call set_buffer(saved, st)
        call css_foreign_completions(css_class_list_function, c)
        ! Completions from `other-buffer-1' and `other-buffer-2' should be
        ! merged.
        call check(error, same_sorted(c, [character(len=3) :: "bar", "baz", "foo"]))
        call kill_buffer(b1, st)
        call kill_buffer(b2, st)
    end subroutine test_foreign_completions

    subroutine test_complete_selector_tag(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=64), allocatable :: c(:)

        call temp_buffer()
        call ins("b")
        c = completions()
        call check(error, any(c == "body"))
        if (allocated(error)) return
        call check(error,.not. any(c == "article"))
    end subroutine test_complete_selector_tag

    subroutine test_complete_selector_class(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: other
        type(status_type) :: st
        character(len=64), allocatable :: c(:)

        call init_bindings()
        call get_buffer_create(" *css-other*", other, st)
        call set_buffer(other, st)
        call css_class_list_function%set_local(foo_bar)
        call temp_buffer()
        call ins(".f")
        c = completions()
        call kill_buffer(other, st)
        call check(error, size(c), 1)
        if (allocated(error)) return
        call check(error, c(1), "foo")
    end subroutine test_complete_selector_class

    subroutine test_complete_selector_id(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: other
        type(status_type) :: st
        character(len=64), allocatable :: c(:)

        call init_bindings()
        call get_buffer_create(" *css-other*", other, st)
        call set_buffer(other, st)
        call css_id_list_function%set_local(foo_bar)
        call temp_buffer()
        call ins("#b")
        c = completions()
        call kill_buffer(other, st)
        call check(error, size(c), 1)
        if (allocated(error)) return
        call check(error, c(1), "bar")
    end subroutine test_complete_selector_id

    subroutine test_complete_nested_selector(error)
        type(error_type), allocatable, intent(out) :: error

        call temp_buffer()
        call ins("body {")
        call check(error,.not. any(completions() == "body"))
    end subroutine test_complete_nested_selector

    subroutine test_scss_complete_nested_selector(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=64), allocatable :: c(:)

        call temp_buffer(scss=.true.)
        call ins("body { b")
        c = completions()
        call check(error, any(c == "body"))
        if (allocated(error)) return
        call check(error,.not. any(c == "article"))
    end subroutine test_scss_complete_nested_selector

    subroutine test_color_to_4_dpc(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, css_color_to_4_dpc("#ffffff"), css_color_to_4_dpc("#fff"))
        if (allocated(error)) return
        call check(error, css_color_to_4_dpc("#aabbcc"), css_color_to_4_dpc("#abc"))
        if (allocated(error)) return
        call check(error, css_color_to_4_dpc("#fab"), "#ffffaaaabbbb")
        if (allocated(error)) return
        call check(error, css_color_to_4_dpc("#fafbfc"), "#fafafbfbfcfc")
    end subroutine test_color_to_4_dpc

    subroutine test_format_hex(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, css_format_hex("#fff"), "#fff")
        if (allocated(error)) return
        call check(error, css_format_hex("#ffffff"), "#fff")
        if (allocated(error)) return
        call check(error, css_format_hex("#aabbcc"), "#abc")
        if (allocated(error)) return
        call check(error, css_format_hex("#12ff34"), "#12ff34")
        if (allocated(error)) return
        call check(error, css_format_hex("#aabbccdd"), "#abcd")
        if (allocated(error)) return
        call check(error, css_format_hex("#aabbccde"), "#aabbccde")
        if (allocated(error)) return
        call check(error, css_format_hex("#abcdef"), "#abcdef")
    end subroutine test_format_hex

    subroutine test_named_color_to_hex(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=6), parameter :: NAMES(3) = [character(len=6) :: "black", "white", "salmon"]
        character(len=7), parameter :: HEX(3) = [character(len=7) :: "#000", "#fff", "#fa8072"]
        logical :: done
        integer :: i

        do i = 1, 3
            call temp_buffer()
            call ins(trim(NAMES(i)))
            done = css_named_color_to_hex()
            call check(error, buffer_string(), trim(HEX(i)))
            if (allocated(error)) return
        end do
    end subroutine test_named_color_to_hex

    subroutine test_format_rgba_alpha(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, css_format_rgba_alpha(0.0_real64), "0")
        if (allocated(error)) return
        call check(error, css_format_rgba_alpha(0.00001_real64), "0")
        if (allocated(error)) return
        call check(error, css_format_rgba_alpha(1.0_real64), "1")
        if (allocated(error)) return
        call check(error, css_format_rgba_alpha(1.00001_real64), "1")
        if (allocated(error)) return
        call check(error, css_format_rgba_alpha(0.10000_real64), "0.1")
        if (allocated(error)) return
        call check(error, css_format_rgba_alpha(0.100001_real64), "0.1")
        if (allocated(error)) return
        call check(error, css_format_rgba_alpha(0.2524334_real64), "0.25")
    end subroutine test_format_rgba_alpha

    subroutine test_hex_to_rgb(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=9), parameter :: INPUTS(8) = [character(len=9) :: "#000", "#000000", "#fff", "#ffffff", &
                                                                                             "#ffffff80", "#fff0", "#fff8", "#ffff"]
        character(len=26), parameter :: OUTPUTS(8) = [character(len=26) :: "rgb(0, 0, 0)", "rgb(0, 0, 0)", &
                                                                                       "rgb(255, 255, 255)", "rgb(255, 255, 255)", &
                                                                             "rgba(255, 255, 255, 0.5)", "rgba(255, 255, 255, 0)", &
                                                                              "rgba(255, 255, 255, 0.53)", "rgba(255, 255, 255, 1)"]
        logical :: done
        integer :: i

        ! (The colors of the initial terminal frame in batch mode.)
        call tty_register_default_colors()

        do i = 1, 8
            call temp_buffer()
            call ins(trim(INPUTS(i)))
            done = css_hex_to_rgb()
            call check(error, buffer_string(), trim(OUTPUTS(i)))
            if (allocated(error)) return
        end do

        call tty_color_clear()
    end subroutine test_hex_to_rgb

    subroutine test_rgb_to_named_color_or_hex(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=25), parameter :: INPUTS(7) = [character(len=25) :: "rgb(0, 0, 0)", "rgb(255, 255, 255)", &
                                                                 "rgb(255, 255, 240)", "rgb(18, 52, 86)", "rgba(18, 52, 86, 0.5)", &
                                                                                "rgba(18, 52, 86, 50%)", "rgba(50%, 50%, 50%, 50%)"]
        character(len=9), parameter :: OUTPUTS(7) = [character(len=9) :: "black", "white", "ivory", "#123456", &
                                                                                              "#12345680", "#12345680", "#80808080"]
        logical :: done
        integer :: i

        do i = 1, 7
            call temp_buffer()
            call ins(trim(INPUTS(i)))
            done = css_rgb_to_named_color_or_hex()
            call check(error, buffer_string(), trim(OUTPUTS(i)))
            if (allocated(error)) return
        end do
    end subroutine test_rgb_to_named_color_or_hex

    subroutine test_cycle_color_format(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call tty_register_default_colors()
        call temp_buffer()
        call ins("black")
        call css_cycle_color_format(st)
        call check(error, buffer_string(), "#000")
        if (allocated(error)) return
        call css_cycle_color_format(st)
        call check(error, buffer_string(), "rgb(0, 0, 0)")
        if (allocated(error)) return
        call css_cycle_color_format(st)
        call check(error, buffer_string(), "black")
        call tty_color_clear()
    end subroutine test_cycle_color_format

    subroutine test_join_nested_selectors(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, css_join_nested_selectors([character(len=7) :: "div", "&:hover"]), "div:hover")
        if (allocated(error)) return
        call check(error, css_join_nested_selectors([character(len=20) :: "a", "&::before, &::after"]), &
                   "a::before, a::after")
        if (allocated(error)) return
        call check(error, css_join_nested_selectors([character(len=16) :: "article", "& > .front-page", "& h1, & h2"]), &
                   "article > .front-page h1, article > .front-page h2")
        if (allocated(error)) return
        call check(error, css_join_nested_selectors([character(len=5) :: ".link", "& + &"]), ".link + .link")
    end subroutine test_join_nested_selectors

    subroutine test_mdn_symbol_guessing(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=32), parameter :: BEFORE(9) = [character(len=32) :: "@med", "@keyframes ", "p::after", &
                                                                                "p:before", "a:v", "border-", "border-color: red", &
                                                                            "border-color: red; color: green", "  border-collapse "]
        character(len=12), parameter :: AFTER(9) = [character(len=12) :: "ia", "{", "", "", "isited", "color: red", &
                                                                                                            ";", ";", ": collapse;"]
        character(len=16), parameter :: EXPECTED(9) = [character(len=16) :: "@media", "@keyframes", "::after", &
                                                                   ":before", ":visited", "border-color", "border-color", "color", &
                                                                                                                  "border-collapse"]
        character(len=:), allocatable :: symbol
        integer(int64) :: here
        logical :: found
        integer :: i

        do i = 1, 9
            call temp_buffer()

            ! (The trailing blanks of "@keyframes " and "  border-collapse "
            ! are significant.)
            if (i == 2) then
                call ins("@keyframes ")
            else if (i == 9) then
                call ins("  border-collapse ")
            else
                call ins(trim(BEFORE(i)))
            end if

            here = point()
            call ins(trim(AFTER(i)))
            call goto_char(here)
            call css_mdn_find_symbol(symbol, found)
            call check(error, found)
            if (allocated(error)) return
            call check(error, symbol, trim(EXPECTED(i)))
            if (allocated(error)) return
        end do
    end subroutine test_mdn_symbol_guessing

    subroutine test_rgb_parser(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=26), parameter :: INPUTS(9) = [character(len=26) :: "255, 0, 128", "255, /* comment */ 0, 128", &
                                                                             "255 0 128", "255, 0, 128, 0.75", "255 0 128 / 0.75", &
                                                                           "100%, 0%, 50%", "100%, 0%, 50%, 0.115", "100% 0% 50%", &
                                                                                                              "100% 0% 50% / 0.115"]
        character(len=:), allocatable :: color
        logical :: found
        integer :: i

        do i = 1, 9
            call temp_buffer()
            call ins(trim(INPUTS(i))//")")
            call goto_char(point_min())
            call css_rgb_color(color, found)
            call check(error, color, "#ff0080")
            if (allocated(error)) return
        end do
    end subroutine test_rgb_parser

    subroutine test_hsl_parser(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=40), parameter :: INPUTS(10) = [character(len=40) :: "0, 100%, 50%", "0 100% 50%", &
                                                                       "0 /* two */ /* comments */100% 50%", "0, 100%, 50%, 0.75", &
                                                                          "0 100% 50% / 0.75", "0deg 100% 50%", "360deg 100% 50%", &
                                                                              "0rad, 100%, 50%, 0.115", "0grad, 100%, 50%, 0.115", &
                                                                                                           "1turn 100% 50% / 0.115"]
        character(len=:), allocatable :: color
        logical :: found
        integer :: i

        do i = 1, 10
            call temp_buffer()
            call ins(trim(INPUTS(i))//")")
            call goto_char(point_min())
            call css_hsl_color(color, found)
            call check(error, color, "#ff0000")
            if (allocated(error)) return
        end do
    end subroutine test_hsl_parser

    subroutine test_hex_color(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, css_hex_color("#abc"), "#abc")
        if (allocated(error)) return
        call check(error, css_hex_color("#abcd"), "#abc")
        if (allocated(error)) return
        call check(error, css_hex_color("#aabbcc"), "#aabbcc")
        if (allocated(error)) return
        call check(error, css_hex_color("#aabbccdd"), "#aabbcc")
    end subroutine test_hex_color

    subroutine test_hex_alpha(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: alpha
        logical :: found

        call css_hex_alpha("#abcd", alpha, found)
        call check(error, alpha, "d")
        if (allocated(error)) return
        call css_hex_alpha("#abc", alpha, found)
        call check(error,.not. found)
        if (allocated(error)) return
        call css_hex_alpha("#aabbccdd", alpha, found)
        call check(error, alpha, "dd")
        if (allocated(error)) return
        call css_hex_alpha("#aabbcc", alpha, found)
        call check(error,.not. found)
    end subroutine test_hex_alpha

    subroutine test_named_color(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=14), parameter :: TEXTS(2) = [character(len=14) :: "@mixin black", "@include black"]
        character(len=:), allocatable :: color
        integer(int64) :: here, start
        logical :: found
        integer :: i

        do i = 1, 2
            call temp_buffer(plain=.true.)
            call ins(trim(TEXTS(i)))
            here = point()
            call backward_word(1_int64)
            start = point()
            call goto_char(here)
            call css_named_color(start, "black", color, found)
            call check(error,.not. found)
            if (allocated(error)) return
        end do
    end subroutine test_named_color

    subroutine test_indent(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer()
        call ins(TEST_INDENT_CSS)
        call indent_region(point_min(), point_max(), st)
        call check(error, buffer_string() == TEST_INDENT_CSS, "test-indent.css changed when reindented")
    end subroutine test_indent

    ! Whether the text of the current buffer, fontified, starts with a
    ! selector.
    logical function selector_at_start()
        call fontify()
        call goto_char(point_min())
        selector_at_start = face() == "css_selector"
    end function selector_at_start

    subroutine test_selectors(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: SEPARATORS(3) = [character(len=3) :: " , ", " > ", " + "]
        character(len=*), parameter :: WRONG(2) = [character(len=3) :: "=", " @ "]
        integer :: i, j, n

        n = size(CSS_SELECTORS)

        do i = 1, n
            call temp_buffer()
            call ins(trim(CSS_SELECTORS(i))//" {"//LF//"}"//LF)
            call check(error, selector_at_start(), "Didn't recognize "//trim(CSS_SELECTORS(i))//" as a selector")
            if (allocated(error)) return
        end do

        ! Test many selectors.
        do i = 1, n
            call temp_buffer()
            call ins(trim(CSS_SELECTORS(i))//" ")

            do j = 1, mod(i, 5)
                call ins(trim(SEPARATORS(mod(i + j, 3) + 1)))
                call ins(" ")
                call ins(trim(CSS_SELECTORS(mod(i*7 + j*3, n) + 1)))
            end do

            call ins("{"//LF//"}"//LF)
            call check(error, selector_at_start(), "Didn't recognize "//text_between(point_min(), &
                                                                                     line_end_position())//" as a selector")
            if (allocated(error)) return
        end do

        ! Test wrong separators.
        do i = 1, n
            call temp_buffer()
            call ins(trim(CSS_SELECTORS(i))//" ")

            do j = 1, mod(i, 5) + 1
                if (mod(i + j, 2) == 0) then
                    call ins("=")
                else
                    call ins(" @ ")
                end if

                call ins(trim(CSS_SELECTORS(mod(i*5 + j*11, n) + 1)))
            end do

            call ins("{"//LF//"}"//LF)
            call check(error,.not. selector_at_start(), "Recognized "//text_between(point_min(), line_end_position()) &
                       //" as a selector")
            if (allocated(error)) return
        end do

        if (.false.) call ins(WRONG(1))
    end subroutine test_selectors

    subroutine test_scss_selectors(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i

        do i = 1, size(SCSS_SELECTORS)
            call temp_buffer(scss=.true.)
            call ins(trim(SCSS_SELECTORS(i))//" {"//LF//"}"//LF)
            call check(error, selector_at_start(), "Didn't recognize "//trim(SCSS_SELECTORS(i))//" as a selector")
            if (allocated(error)) return
        end do
    end subroutine test_scss_selectors

    ! Own: a color gets its own background.
    subroutine test_fontify_colors(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: v
        type(value_type), allocatable :: elts(:)
        type(status_type) :: st

        call temp_buffer()
        call ins("p { color: #ff0000; }")
        call fontify()
        call goto_char(13_int64)
        call get_text_property(point(), sym("face"), v, st)
        call check(error, consp(v), "no anonymous face on the color")
        if (allocated(error)) return
        call list_elements(v, elts)
        call check(error, xstring(elts(2)), "#ff0000")
    end subroutine test_fontify_colors

    ! Own: M-q reformats a declaration block (the results of Emacs 31.1).
    subroutine test_fill_block(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer()
        call ins("a { b: c; d: e; }")
        call goto_char(3_int64)
        call fill_paragraph(st)
        call check(error, buffer_string(), "a { "//LF//"    b: c; "//LF//"    d: e; "//LF//"}")
    end subroutine test_fill_block

    ! Own: M-q fills a comment.
    subroutine test_fill_comment(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer()
        call ins("/* aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll mmm nnn ooo ppp qqq rrr sss ttt uuu vvv */")
        call goto_char(5_int64)
        call fill_paragraph(st)
        call check(error, buffer_string(), "/* aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll mmm nnn ooo ppp qqq"//LF &
                   //"   rrr sss ttt uuu vvv */")
    end subroutine test_fill_comment
end module test_css_mode_fm
