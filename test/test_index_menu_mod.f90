! test_index_menu_mod.f90 -- tests for Imenu (index_menu_mod) and the modes' indexes.
! Ports GNU Emacs 31.1 test/lisp/imenu-tests.el.
!
! Copyright (C) 2013-2026 Free Software Foundation, Inc.
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

!> Ports imenu-tests.el (all 3 tests) and own tests of the index, the
!> cache, the command and the indexes of the ported modes.  Every test
!> makes its buffer and closes it before returning.
module test_index_menu_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error, FM_IMENU_UNAVAILABLE
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: point, point_min, point_max, goto_char
    use search_mod, only: search_forward
    use bindings_mod, only: init_bindings
    use keyboard_mod, only: kbd_store_bytes, discard_input, command_loop
    use test_support_mod, only: show_alone
    use simple_mod, only: fundamental_mode
    use index_menu_mod, only: imenu, imenu_index_type, imenu_entry_type, imenu_pattern_type, imenu_create_index_procedure, &
                              imenu_create_index_function, imenu_patterns, make_imenu_index, &
                              make_imenu_generic_index, flush_imenu_cache, has_imenu_index, find_imenu_default, &
                              find_imenu_entry, flatten_imenu_index, truncate_imenu_items, is_imenu_position_before, &
                              is_imenu_name_before, imenu_max_item_length, imenu_auto_rescan, imenu_use_markers, &
                              imenu_generic_skip_comments_and_strings, IMENU_RESCAN_NAME, IMENU_RESCAN_POSITION
    use shell_script_mod, only: sh_mode
    use configuration_mode_mod, only: conf_unix_mode, conf_space_mode, conf_colon_mode, conf_ppd_mode, conf_javaprop_mode
    use make_mode_mod, only: makefile_mode, makefile_makepp_mode
    use f90_mod, only: f90_mode
    use diff_mode_mod, only: diff_mode
    use sgml_mode_mod, only: sgml_mode, html_mode
    use css_mode_mod, only: css_mode
    use python_mod, only: python_mode
    implicit none(type, external)
    private

    public :: collect_index_menu_mod

    character(len=*), parameter :: LF = achar(10), TAB = achar(9), CR = achar(13)

contains

    !> Collect all exported unit tests.
    subroutine collect_index_menu_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("imenu-simple-scan-deftest-sh", test_scan_sh), &
                    new_unittest("imenu--sort-by-position-pairs", test_sort_by_position_pairs), &
                    new_unittest("imenu--sort-by-position-list", test_sort_by_position_list), &
                    new_unittest("imenu-generic-submenus", test_generic_submenus), &
                    new_unittest("imenu-generic-comments-and-duplicates", test_generic_comments), &
                    new_unittest("imenu-make-index-rescan-and-cache", test_make_index), &
                    new_unittest("imenu-unavailable", test_unavailable), &
                    new_unittest("imenu-goto-by-name", test_goto_by_name), &
                    new_unittest("imenu-command-reads-entry", test_command), &
                    new_unittest("imenu-find-default-and-entry", test_find_default), &
                    new_unittest("imenu-flatten-and-truncate", test_flatten), &
                    new_unittest("imenu-sort-by-name", test_sort_by_name), &
                    new_unittest("imenu-conf-modes", test_conf), &
                    new_unittest("imenu-makefile-mode", test_makefile), &
                    new_unittest("imenu-f90-mode", test_f90), &
                    new_unittest("imenu-diff-mode", test_diff), &
                    new_unittest("imenu-sgml-and-html-modes", test_sgml), &
                    new_unittest("imenu-css-mode", test_css), &
                    new_unittest("imenu-python-mode", test_python) &
                    ]
    end subroutine collect_index_menu_mod

    !> A fresh buffer holding TEXT (with-temp-buffer, insert).
    subroutine temp_buffer(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call get_or_make_buffer(" *imenu-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
    end subroutine temp_buffer

    !> INDEX made by the current buffer's `imenu_create_index_function'.
    subroutine create_index(index, status)
        type(imenu_index_type), intent(out) :: index
        type(status_type), intent(out) :: status
        procedure(imenu_create_index_procedure), pointer :: create

        create => imenu_create_index_function%current()
        call create(index, status)
    end subroutine create_index

    !> The names of INDEX (its entries and submenus) in order, each
    !> followed by "|", submenu entries' names with the submenu's and "/".
    function names_of(index) result(s)
        type(imenu_index_type), intent(in) :: index
        character(len=:), allocatable :: s
        integer :: k

        s = ""
        if (.not. allocated(index%entries)) return

        do k = 1, size(index%entries)
            if (index%entries(k)%parent > 0) s = s//index%entries(index%entries(k)%parent)%name//"/"
            s = s//index%entries(k)%name//"|"
        end do
    end function names_of

    !> imenu-simple-scan-deftest-sh: the index of a shell script has the
    !> four functions (compared sorted, as the test does; here in order).
    subroutine test_scan_sh(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(imenu_index_type) :: index
        type(status_type) :: st

        call temp_buffer("a()"//LF//"{"//LF//"}"//LF//"function b"//LF//"{"//LF//"}"//LF//"function c()"//LF//"{"//LF// &
                         "}"//LF//"function ABC_D()"//LF//"{"//LF//"}"//LF, h)
        call sh_mode(st)
        call create_index(index, st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, names_of(index), "a|b|c|ABC_D|")
        call close_buffer(h, st)
    end subroutine test_scan_sh

    !> imenu--sort-by-position-pairs.
    subroutine test_sort_by_position_pairs(error)
        type(error_type), allocatable, intent(out) :: error
        type(imenu_entry_type) :: a2, a3

        a2%name = "a"
        a2%position = 2
        a3%name = "a"
        a3%position = 3
        call check(error, is_imenu_position_before(a2, a3), "2 before 3")
        if (allocated(error)) return
        call check(error,.not. is_imenu_position_before(a3, a2), "3 not before 2")
    end subroutine test_sort_by_position_pairs

    !> imenu--sort-by-position-list: the same for special items (FUNCTION
    !> and ARGUMENTS are not ported, so they are the same entries).
    subroutine test_sort_by_position_list(error)
        type(error_type), allocatable, intent(out) :: error
        type(imenu_entry_type) :: a2, a3

        a2%name = "a"
        a2%position = 2
        a3%name = "a"
        a3%position = 3
        call check(error, is_imenu_position_before(a2, a3), "2 before 3")
        if (allocated(error)) return
        call check(error,.not. is_imenu_position_before(a3, a2), "3 not before 2")
    end subroutine test_sort_by_position_list

    !> The generic function: submenus come first (the last made first),
    !> then the top-level items, each sorted by position; a position is
    !> the beginning of the line where the name is.
    subroutine test_generic_submenus(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(imenu_index_type) :: index
        type(imenu_pattern_type) :: patterns(3)
        type(status_type) :: st

        call temp_buffer("var x"//LF//"fun f"//LF//"var y"//LF//"top t"//LF//"fun g"//LF, h)
        call fundamental_mode(st)
        patterns(1) = imenu_pattern_type(menu_title="Variables", regexp="^var \(\w+\)", subexpression=1)
        patterns(2) = imenu_pattern_type(menu_title="Functions", regexp="^fun \(\w+\)", subexpression=1)
        patterns(3) = imenu_pattern_type(menu_title="", regexp="^top \(\w+\)", subexpression=1)
        call make_imenu_generic_index(patterns, index, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, names_of(index), "Functions|Functions/f|Functions/g|Variables|Variables/x|Variables/y|t|")
        if (allocated(error)) return
        call check(error, index%position_of(3) == 25, "g at the beginning of its line")
        call close_buffer(h, st)
    end subroutine test_generic_submenus

    !> Definitions in comments are skipped (unless
    !> `imenu_generic_skip_comments_and_strings' is false), and an item
    !> found twice is kept once.
    subroutine test_generic_comments(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(imenu_index_type) :: index
        type(imenu_pattern_type) :: patterns(2)
        type(status_type) :: st

        call temp_buffer("f()"//LF//"# g()"//LF//"h()"//LF, h)
        call sh_mode(st)
        patterns(1) = imenu_pattern_type(menu_title="", regexp="\(\w+\)()", subexpression=1)
        patterns(2) = imenu_pattern_type(menu_title="", regexp="^\(\w+\)()", subexpression=1)
        call make_imenu_generic_index(patterns, index, st)
        call check(error, names_of(index), "f|h|")
        if (allocated(error)) return
        imenu_generic_skip_comments_and_strings = .false.
        call make_imenu_generic_index(patterns, index, st)
        imenu_generic_skip_comments_and_strings = .true.
        call check(error, names_of(index), "f|g|h|")
        call close_buffer(h, st)
    end subroutine test_generic_comments

    !> `make_imenu_index' puts the rescan entry first and caches the
    !> index (with markers), which a rescan or `flush_imenu_cache' renews;
    !> `imenu_auto_rescan' rescans every time and adds no rescan entry.
    subroutine test_make_index(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(imenu_index_type) :: index
        type(status_type) :: st

        call temp_buffer("f()"//LF//"g()"//LF, h)
        call sh_mode(st)
        call make_imenu_index(index, st)

        body: block
            call check(error, names_of(index), IMENU_RESCAN_NAME//"|f|g|")
            if (allocated(error)) exit body
            call check(error, index%entries(1)%position == IMENU_RESCAN_POSITION, "the rescan position")
            if (allocated(error)) exit body
            call check(error, has_imenu_index(), "cached")
            if (allocated(error)) exit body
            call check(error, index%entries(3)%marker%index /= 0, "a marker")
            if (allocated(error)) exit body
            ! The marker follows the text.
            call goto_char(point_min())
            call insert("x=1"//LF, st)
            call check(error, index%position_of(3) == 9, "g moved with its marker")
            if (allocated(error)) exit body
            call goto_char(point_max())
            call insert("k()"//LF, st)
            call make_imenu_index(index, st)
            call check(error, names_of(index), IMENU_RESCAN_NAME//"|f|g|", "the cached index")
            if (allocated(error)) exit body
            call flush_imenu_cache()
            call check(error,.not. has_imenu_index(), "flushed")
            if (allocated(error)) exit body
            imenu_auto_rescan = .true.
            call make_imenu_index(index, st)
            imenu_auto_rescan = .false.
            call check(error, names_of(index), "f|g|k|", "rescanned")
            if (allocated(error)) exit body
            imenu_use_markers = .false.
            call flush_imenu_cache()
            call make_imenu_index(index, st)
            imenu_use_markers = .true.
            call check(error, index%entries(2)%marker%index == 0, "no marker")
        end block body

        imenu_auto_rescan = .false.
        imenu_use_markers = .true.
        call close_buffer(h, st)
    end subroutine test_make_index

    !> A buffer with nothing to index: imenu_unavailable, unless NOERROR.
    subroutine test_unavailable(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(imenu_index_type) :: index
        type(status_type) :: st

        call temp_buffer("nothing here"//LF, h)
        call fundamental_mode(st)
        call make_imenu_index(index, st)
        call check(error, st%code == FM_IMENU_UNAVAILABLE, "unavailable")
        if (allocated(error)) return
        call check(error, format_error(st), &
                   "imenu unavailable: ""This buffer cannot use `create_imenu_index'""")
        if (allocated(error)) return
        call sh_mode(st)
        call make_imenu_index(index, st)
        call check(error, format_error(st), "imenu unavailable: ""No items suitable for an index found in this buffer""")
        if (allocated(error)) return
        call make_imenu_index(index, st, noerror=.true.)
        call check(error, st%ok() .and. has_imenu_index(), "an empty index with NOERROR")
        if (allocated(error)) return
        call check(error, names_of(index), IMENU_RESCAN_NAME//"|")
        call close_buffer(h, st)
    end subroutine test_unavailable

    !> (imenu "NAME") goes to the entry's position, widening if needed.
    subroutine test_goto_by_name(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer("f()"//LF//"  g()"//LF//"h()"//LF, h)
        call sh_mode(st)
        call goto_char(point_max())
        call imenu(st, name="g")
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, point() == 5_int64, "at g's line")
        call close_buffer(h, st)
    end subroutine test_goto_by_name

    !> M-g i reads an entry with completion (the default being the name at
    !> point) and goes there; an entry of a submenu is read in turn.
    subroutine test_command(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: found

        call temp_buffer("[one]"//LF//"a = 1"//LF//"[two]"//LF//"b = 2"//LF//"two"//LF, h)
        call conf_unix_mode(st)
        call show_alone()
        call discard_input()
        call goto_char(point_min())
        call kbd_store_bytes(achar(27)//"gitwo"//CR)
        call command_loop(st)

        body: block
            call check(error, point() == 13_int64, "at [two]")
            if (allocated(error)) exit body
            ! Parameters is a submenu.
            call kbd_store_bytes(achar(27)//"giParameters"//CR//"b"//CR)
            call command_loop(st)
            call check(error, point() == 19_int64, "at b")
            if (allocated(error)) exit body
            ! The name at point is the default.
            call goto_char(point_min())
            call search_forward("two", found, st, count=2_int64)
            call goto_char(point() - 1)
            call kbd_store_bytes(achar(27)//"gi"//CR)
            call command_loop(st)
            call check(error, point() == 13_int64, "the default")
        end block body

        call discard_input()
        call close_buffer(h, st)
    end subroutine test_command

    !> imenu-find-default's fuzzy matches and imenu--in-alist.
    subroutine test_find_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(imenu_index_type) :: index
        integer :: k

        call index%append("Functions", k, submenu=.true.)
        call index%append("foo_bar", k, position=10_int64, parent=1)
        call index%append("Bar", k, position=20_int64)
        call index%append("xbarx", k, position=30_int64)
        call check(error, find_imenu_default("Bar", index), "Bar")
        if (allocated(error)) return
        call check(error, find_imenu_default("bar", index), "Bar", "exact, case folded")
        if (allocated(error)) return
        call check(error, find_imenu_default("barx", index), "xbarx", "a suffix")
        if (allocated(error)) return
        call check(error, find_imenu_default("arx", index), "xbarx", "anywhere")
        if (allocated(error)) return
        call check(error, find_imenu_default("zzz", index), "", "none")
        if (allocated(error)) return
        call check(error, find_imenu_entry("foo_bar", index) == 2, "in a submenu")
        if (allocated(error)) return
        call check(error, find_imenu_entry("Functions", index) == 0, "not a submenu")
    end subroutine test_find_default

    !> imenu--flatten-index-alist with `imenu_level_separator', and
    !> imenu--truncate-items.
    subroutine test_flatten(error)
        type(error_type), allocatable, intent(out) :: error
        type(imenu_index_type) :: index, flat
        integer :: k, saved

        call index%append("Types", k, submenu=.true.)
        call index%append("Inner", k, parent=1, submenu=.true.)
        call index%append("t1", k, position=10_int64, parent=2)
        call index%append("t2", k, position=20_int64, parent=1)
        call index%append("main", k, position=30_int64)
        flat = flatten_imenu_index(index, .true.)
        call check(error, names_of(flat), "Types:Inner:t1|Types:t2|main|")
        if (allocated(error)) return
        call check(error, flat%position_of(2) == 20_int64, "positions kept")
        if (allocated(error)) return
        saved = imenu_max_item_length
        imenu_max_item_length = 3
        call truncate_imenu_items(flat)
        imenu_max_item_length = saved
        call check(error, names_of(flat), "Typ|Typ|mai|")
    end subroutine test_flatten

    !> imenu--sort-by-name.
    subroutine test_sort_by_name(error)
        type(error_type), allocatable, intent(out) :: error
        type(imenu_entry_type) :: a, ab, b

        a%name = "a"
        ab%name = "ab"
        b%name = "b"
        call check(error, is_imenu_name_before(a, ab) .and. is_imenu_name_before(ab, b), "a < ab < b")
        if (allocated(error)) return
        call check(error,.not. is_imenu_name_before(b, a) .and. .not. is_imenu_name_before(a, a), "not before")
    end subroutine test_sort_by_name

    !> Conf mode's sections and parameters, and its variants'.
    subroutine test_conf(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(imenu_index_type) :: index
        type(status_type) :: st

        call temp_buffer("[sec]"//LF//"key = v"//LF//"block {"//LF//"}"//LF, h)
        call conf_unix_mode(st)
        call create_index(index, st)

        body: block
            ! The name of a "section {" keeps its spaces, as upstream's regexp
            ! has them in its group.
            call check(error, names_of(index), "Parameters|Parameters/key|sec|block |")
            if (allocated(error)) exit body
            call erase_buffer(st)
            call insert("a.b: 1"//LF//"c=2"//LF, st)
            call conf_javaprop_mode(st)
            call create_index(index, st)
            call check(error, names_of(index), "Parameters|Parameters/a.b|Parameters/c|", "javaprop")
            if (allocated(error)) exit body
            call erase_buffer(st)
            call insert("*Key: v"//LF//"[x]"//LF, st)
            call conf_colon_mode(st)
            call create_index(index, st)
            call check(error, names_of(index), "Parameters|Parameters/*Key|x|", "colon")
            if (allocated(error)) exit body
            call conf_ppd_mode(st)
            call create_index(index, st)
            call check(error, names_of(index), "Parameters|Parameters/*Key|", "ppd")
            if (allocated(error)) exit body
            call erase_buffer(st)
            call insert("image/png png"//LF//"[s]"//LF, st)
            call conf_space_mode(st)
            call create_index(index, st)
            call check(error, names_of(index), "Parameters|Parameters/image/png|s|", "space")
        end block body

        call close_buffer(h, st)
    end subroutine test_conf

    !> Makefile mode's dependencies (found by `makefile_previous_dependency')
    !> and macro assignments; makepp's functions.
    subroutine test_makefile(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(imenu_index_type) :: index
        type(status_type) :: st

        call temp_buffer("CC = gcc"//LF//"all: foo"//LF//TAB//"cc foo"//LF//"foo: bar"//LF//"sub f {"//LF, h)
        call makefile_mode(st)
        call create_index(index, st)

        body: block
            call check(error, names_of(index), "Macro Assignment|Macro Assignment/CC|Dependencies|Dependencies/all|"// &
                       "Dependencies/foo|")
            if (allocated(error)) exit body
            call makefile_makepp_mode(st)
            call create_index(index, st)
            call check(error, names_of(index), "Macro Assignment|Macro Assignment/CC|Dependencies|Dependencies/all|"// &
                       "Dependencies/foo|Functions|Functions/f|", "makepp")
        end block body

        call close_buffer(h, st)
    end subroutine test_makefile

    !> F90 mode's program, modules, types (`f90_imenu_type_matcher') and
    !> procedures, with case folded.
    subroutine test_f90(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(imenu_index_type) :: index
        type(status_type) :: st

        call temp_buffer("MODULE m"//LF//"type :: point"//LF//"end type point"//LF//"type, public :: box"//LF// &
                         "end type"//LF//"type(point) :: p"//LF//"contains"//LF//"subroutine s(x)"//LF// &
                         "end subroutine s"//LF//"integer function f()"//LF//"end function f"//LF//"end module m"//LF// &
                         "program main"//LF//"end program main"//LF, h)
        call f90_mode(st)
        call create_index(index, st)
        call check(error, names_of(index), "Procedures|Procedures/s|Procedures/f|Types|Types/point|Types/box|"// &
                   "Modules|Modules/m|main|")
        call close_buffer(h, st)
    end subroutine test_f90

    !> Diff mode's file names (the second name of a unified diff).
    subroutine test_diff(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(imenu_index_type) :: index
        type(status_type) :: st

        call temp_buffer("--- a/x.f90"//TAB//"date"//LF//"+++ b/x.f90"//TAB//"date"//LF//"@@ -1 +1 @@"//LF//"-a"//LF// &
                         "+b"//LF//"--- a/y.f90"//LF//"+++ b/y.f90"//LF//"@@ -1 +1 @@"//LF//"-a"//LF//"+b"//LF, h)
        call diff_mode(st)
        call create_index(index, st)
        call check(error, names_of(index), "b/x.f90|b/y.f90|")
        call close_buffer(h, st)
    end subroutine test_diff

    !> SGML mode's ids and names; HTML mode's headings (indented by level).
    subroutine test_sgml(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(imenu_index_type) :: index
        type(status_type) :: st

        call temp_buffer("<!ELEMENT para - - (#PCDATA)>"//LF//"<p id=""one"">x</p>"//LF//"<a name=two>y</a>"//LF, h)
        call sgml_mode(st)
        call create_index(index, st)

        body: block
            call check(error, names_of(index), "Name|Name/two|Id|Id/one|para|")
            if (allocated(error)) exit body
            call erase_buffer(st)
            call insert("<h1>Title</h1>"//LF//"<h2><b>Sub</b></h2>"//LF//"<h3>Deep</h3>"//LF, st)
            call html_mode(st)
            call create_index(index, st)
            call check(error, names_of(index), "Title|  Sub|    Deep|", "html")
            if (allocated(error)) exit body
            call check(error, index%position_of(3) == 36_int64, "a heading's line")
        end block body

        call close_buffer(h, st)
    end subroutine test_sgml

    !> CSS mode's index of rules, through its
    !> `imenu_prev_index_position_function' and
    !> `imenu_extract_index_name_function'.
    subroutine test_css(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(imenu_index_type) :: index
        type(status_type) :: st

        call temp_buffer("a { color: red; }"//LF//"/* c */"//LF//"p.x , em { }"//LF, h)
        call css_mode(st)
        call create_index(index, st)
        call check(error, names_of(index), "a|p.x , em|")
        call close_buffer(h, st)
    end subroutine test_css

    !> Python mode's tree: a class with its methods, after a function.
    subroutine test_python(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(imenu_index_type) :: index
        type(status_type) :: st

        call temp_buffer("def f():"//LF//"    pass"//LF//"class C:"//LF//"    def m(self):"//LF//"        pass"//LF// &
                         "    def n(self):"//LF//"        pass"//LF//"def g():"//LF//"    pass"//LF, h)
        call python_mode(st)
        call create_index(index, st)
        call check(error, names_of(index), "f (def)|C (class)|C (class)/*class definition*|C (class)/m (def)|"// &
                   "C (class)/n (def)|g (def)|")
        call close_buffer(h, st)
    end subroutine test_python
end module test_index_menu_mod
