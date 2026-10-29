! test_ibuf_ext_fm.f90 -- tests for the Ibuffer filters (ibuf_ext_fm).
! Ported from GNU Emacs 31.1 test/lisp/ibuffer-tests.el.
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

!> Ports the filter tests of ibuffer-tests.el: ibuffer-test-Bug24997,
!> ibuffer-save-filters, ibuffer-test-Bug25058, ibuffer-filter-inclusion-1
!> to -8, ibuffer-decompose-filter, ibuffer-and-filter, ibuffer-or-filter,
!> ibuffer-format-qualifier and ibuffer-unary-operand (only its (not
!> FILTER) form exists).  Modes that are not ported are replaced by ported
!> ones: emacs-lisp-mode by sh_mode (also derived from prog_mode, with
!> ".sh" files), html-mode and the "web" saved filters by sh_mode and
!> "programming", artist-mode by fundamental_mode and text_mode.
!> ibuffer-test-Bug25042 (a predicate form that signals an error) is not
!> applicable: a predicate is a Fortran function.  Own tests of filter
!> groups and of `completing_read_multiple' (crm.el has no tests).
module test_ibuf_ext_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_string, make_list, make_fixnum
    use buffer_fm, only: buffers, current_buffer, set_buffer, kill_buffer, get_buffer_create, get_buffer, &
                         buffer_live_p, generate_new_buffer_name, restore_buffer_modified_p
    use insdel_fm, only: insert
    use editfns_fm, only: point_min, point_max, buffer_string
    use search_fm, only: regexp_quote
    use files_fm, only: make_temp_file, find_file_noselect, delete_file
    use simple_fm, only: fundamental_mode
    use text_mode_fm, only: text_mode
    use sh_script_fm, only: sh_mode
    use keymap_fm, only: key_parse
    use keyboard_fm, only: unread_command_events, discard_input
    use minibuf_fm, only: minibuffer_string_type
    use minibuf_table_fm, only: completion_table_type, TABLE_LIST
    use crm_fm, only: completing_read_multiple
    use bindings_fm, only: init_bindings
    use ibuffer_fm, only: ibuffer, ibuffer_update, ibuffer_included_in_filters_p, ibuffer_unary_operand, &
                          ibuffer_format_qualifier, ibuffer_save_filters, ibuffer_saved_filters, ibuffer_decompose_filter, &
                          ibuffer_and_filter, ibuffer_or_filter, ibuffer_filter_disable, ibuffer_saved_filter_groups, &
                          ibuffer_switch_to_saved_filter_groups, ibuffer_decompose_filter_group, ibuffer_save_with_custom, &
                          ibuffer_filtering_qualifiers, ibuffer_set_filtering_qualifiers, ibuffer_filter_by_name, &
                          ibuffer_filters_to_filter_group, ibuffer_filter_groups, ibuffer_kill_filter_group, &
                          ibuffer_yank_filter_group, ibuffer_pop_filter, ibuffer_expert
    use ibuf_macs_fm, only: ibuffer_filter_type, ibuffer_saved_filters_type, ibuffer_saved_filter_groups_type, &
                            ibuffer_filter_groups_type, ibuffer_and, ibuffer_or, ibuffer_not, ibuffer_saved, &
                            ibuffer_major_mode, ibuffer_used_mode, ibuffer_derived_mode, ibuffer_name, ibuffer_starred_name, &
                            ibuffer_filename, ibuffer_basename, ibuffer_file_extension, ibuffer_directory, ibuffer_size_gt, &
                            ibuffer_size_lt, ibuffer_modified, ibuffer_visiting_file, ibuffer_content, ibuffer_predicate, &
                            ibuffer_filter_equal
    implicit none(type, external)
    private

    public :: collect_ibuf_ext_fm

    character(len=*), parameter :: LF = achar(10)

    ! Modes of the test buffers.
    integer, parameter :: FUNDAMENTAL = 0, TEXT = 1, SH = 2

    ! Accumulated buffers and files to clean up.
    type(handle_type) :: test_buffers(16)
    integer :: ntest_buffers = 0
    type(minibuffer_string_type) :: test_files(16)
    integer :: ntest_files = 0

    ! The status of the last `incl'.
    type(status_type) :: last_status

contains

    subroutine collect_ibuf_ext_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("ibuffer-test-Bug24997", test_bug24997), &
                    new_unittest("ibuffer-save-filters", test_save_filters), &
                    new_unittest("ibuffer-test-Bug25058", test_bug25058), &
                    new_unittest("ibuffer-filter-inclusion-1", test_inclusion_1), &
                    new_unittest("ibuffer-filter-inclusion-2", test_inclusion_2), &
                    new_unittest("ibuffer-filter-inclusion-3", test_inclusion_3), &
                    new_unittest("ibuffer-filter-inclusion-4", test_inclusion_4), &
                    new_unittest("ibuffer-filter-inclusion-5", test_inclusion_5), &
                    new_unittest("ibuffer-filter-inclusion-6", test_inclusion_6), &
                    new_unittest("ibuffer-filter-inclusion-7", test_inclusion_7), &
                    new_unittest("ibuffer-filter-inclusion-8", test_inclusion_8), &
                    new_unittest("ibuffer-decompose-filter", test_decompose_filter), &
                    new_unittest("ibuffer-and-filter", test_and_filter), &
                    new_unittest("ibuffer-or-filter", test_or_filter), &
                    new_unittest("ibuffer-format-qualifier", test_format_qualifier), &
                    new_unittest("ibuffer-unary-operand", test_unary_operand), &
                    new_unittest("ibuffer-filter-groups", test_filter_groups), &
                    new_unittest("completing-read-multiple", test_crm) &
                    ]
    end subroutine collect_ibuf_ext_fm

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    ! Whether BUF passes the filters F1, F2, ... (`last_status' holds the
    ! status).
    logical function incl(buf, f1, f2, f3)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_type), intent(in) :: f1
        type(ibuffer_filter_type), intent(in), optional :: f2, f3
        type(ibuffer_filter_type), allocatable :: fs(:)
        integer :: n
        n = 1
        if (present(f2)) n = n + 1
        if (present(f3)) n = n + 1
        allocate (fs(n))
        fs(1) = f1
        if (present(f2)) fs(2) = f2
        if (present(f3)) fs(n) = f3
        incl = ibuffer_included_in_filters_p(buf, fs, last_status)
    end function incl

    ! (should (ibuffer-included-in-filters-p ...)): COND without error.
    subroutine should(error, cond, what)
        type(error_type), allocatable, intent(out) :: error
        logical, intent(in) :: cond
        character(len=*), intent(in) :: what
        call check(error, last_status%ok(), what//": "//format_error(last_status))
        if (allocated(error)) return
        call check(error, cond, what)
    end subroutine should

    ! Set the current buffer's major mode to MODE.
    subroutine set_mode(mode)
        integer, intent(in) :: mode
        type(status_type) :: st
        select case (mode)
        case (TEXT)
            call text_mode(st)
        case (SH)
            call sh_mode(st)
        case default
            call fundamental_mode(st)
        end select
    end subroutine set_mode

    ! Add exactly SIZE characters (0 for none) to BUF, including INCLUDE.
    subroutine set_buffer_contents(buf, size, include)
        type(handle_type), intent(in) :: buf
        integer, intent(in) :: size
        character(len=*), intent(in) :: include
        character(len=*), parameter :: CHUNK = "ccccccccccccccccccccccccccccccc"//LF
        type(handle_type) :: old
        type(status_type) :: st
        integer :: n, k
        old = current_buffer()
        call set_buffer(buf, st)
        call insert(include, st)
        n = size - len(include)
        if (size > 0) then
            do k = 1, n/len(CHUNK)
                call insert(CHUNK, st)
            end do
            do k = 1, mod(n, len(CHUNK))
                call insert(LF, st)
            end do
        end if
        ! prevent query on cleanup
        call restore_buffer_modified_p(.false.)
        call set_buffer(old, st)
    end subroutine set_buffer_contents

    ! Create a file and buffer with designated properties: the file name
    ! begins with PREFIX and ends with SUFFIX.
    function create_file_buffer(prefix, suffix, size, include, mode) result(buf)
        character(len=*), intent(in) :: prefix, suffix, include
        integer, intent(in) :: size, mode
        type(handle_type) :: buf, old
        character(len=:), allocatable :: file
        type(status_type) :: st
        call make_temp_file(prefix, file, st)
        ntest_files = ntest_files + 1
        test_files(ntest_files)%s = file
        call find_file_noselect(file//suffix, buf, st)
        ntest_buffers = ntest_buffers + 1
        test_buffers(ntest_buffers) = buf
        old = current_buffer()
        call set_buffer(buf, st)
        call set_mode(mode)
        call set_buffer(old, st)
        call set_buffer_contents(buf, size, include)
    end function create_file_buffer

    ! Create a non-file buffer with designated properties.
    function create_non_file_buffer(prefix, size, include, mode) result(buf)
        character(len=*), intent(in) :: prefix, include
        integer, intent(in) :: size, mode
        type(handle_type) :: buf, old
        type(status_type) :: st
        call get_buffer_create(generate_new_buffer_name(prefix), buf, st)
        ntest_buffers = ntest_buffers + 1
        test_buffers(ntest_buffers) = buf
        old = current_buffer()
        call set_buffer(buf, st)
        call set_mode(mode)
        call set_buffer(old, st)
        call set_buffer_contents(buf, size, include)
    end function create_non_file_buffer

    ! Restore all state modified during the tests.
    subroutine clean_up()
        type(status_type) :: st
        integer :: k
        do k = 1, ntest_files
            call delete_file(test_files(k)%s, st)
        end do
        ntest_files = 0
        do k = ntest_buffers, 1, -1
            if (buffer_live_p(test_buffers(k))) call kill_buffer(test_buffers(k), st)
        end do
        ntest_buffers = 0
    end subroutine clean_up

    ! The `default_directory' of BUF (the test runner's buffers may have
    ! none: then "/", set as the directory of BUF).
    function directory_of(buf) result(dir)
        type(handle_type), intent(in) :: buf
        character(len=:), allocatable :: dir
        if (.not. allocated(buffers(buf%index)%directory)) buffers(buf%index)%directory = "/"
        dir = buffers(buf%index)%directory
    end function directory_of

    ! The current buffer's text is longer than 18 characters.
    recursive logical function longer_than_18(buffer)
        type(handle_type), intent(in) :: buffer
        if (buffer%index < 0) continue
        longer_than_18 = point_max() - point_min() > 18
    end function longer_than_18

    ! Whether the filter lists A and B are `equal'.
    logical function same_filters(a, b)
        type(ibuffer_filter_type), intent(in) :: a(:), b(:)
        integer :: k
        same_filters = size(a) == size(b)
        if (.not. same_filters) return
        do k = 1, size(a)
            if (.not. ibuffer_filter_equal(a(k), b(k))) same_filters = .false.
        end do
    end function same_filters

    ! Start a test Ibuffer buffer *Test-Ibuffer* and make it current.
    subroutine get_test_ibuffer(old, ib, error)
        type(handle_type), intent(out) :: old, ib
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call init_bindings()
        ibuffer_expert = .true.
        old = current_buffer()
        call ibuffer(st, name="*Test-Ibuffer*", noselect=.true.)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        ib = get_buffer("*Test-Ibuffer*")
        call set_buffer(ib, st)
    end subroutine get_test_ibuffer

    subroutine kill_test_ibuffer(old, ib)
        type(handle_type), intent(in) :: old, ib
        type(status_type) :: st
        if (buffer_live_p(ib)) then
            call set_buffer(ib, st)
            call restore_buffer_modified_p(.false.)
            call kill_buffer(ib, st)
        end if
        if (buffer_live_p(old)) call set_buffer(old, st)
    end subroutine kill_test_ibuffer

    ! Set the filters of the current Ibuffer buffer to F1, F2, ... (the
    ! top first); none for no filters.
    subroutine set_filters(f1, f2)
        type(ibuffer_filter_type), intent(in), optional :: f1, f2
        type(ibuffer_filter_type), allocatable :: fs(:)
        integer :: n
        n = 0
        if (present(f1)) n = n + 1
        if (present(f2)) n = n + 1
        allocate (fs(n))
        if (present(f1)) fs(1) = f1
        if (present(f2)) fs(n) = f2
        call ibuffer_set_filtering_qualifiers(fs)
    end subroutine set_filters

    ! ------------------------------------------------------------------
    ! Tests
    ! ------------------------------------------------------------------

    !> Test for https://debbugs.gnu.org/24997 .
    subroutine test_bug24997(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, ib
        type(status_type) :: st
        type(ibuffer_filter_type), allocatable :: filters(:)
        call get_test_ibuffer(old, ib, error)
        if (allocated(error)) return
        call set_filters(ibuffer_size_gt(10_int64), ibuffer_used_mode("lisp_interaction_mode"))
        call ibuffer_update(st, .false., .true.)
        call ibuffer_decompose_filter(st)
        filters = ibuffer_filtering_qualifiers()
        call check(error, size(filters) > 1, "filters lost")
        call set_filters()
        call kill_test_ibuffer(old, ib)
    end subroutine test_bug24997

    !> Tests that `ibuffer_save_filters' saves in the proper format.
    subroutine test_save_filters(error)
        type(error_type), allocatable, intent(out) :: error
        type(ibuffer_saved_filters_type) :: orig
        type(ibuffer_filter_type) :: test1(2), test2(1), test3(3)
        type(status_type) :: st
        logical :: saved_custom
        call body()
        ibuffer_saved_filters = orig
        ibuffer_save_with_custom = saved_custom
    contains
        subroutine body()
            saved_custom = ibuffer_save_with_custom
            orig = ibuffer_saved_filters
            ibuffer_save_with_custom = .false.
            call ibuffer_saved_filters%clear()
            test1(1) = ibuffer_major_mode("org_mode")
            test1(2) = ibuffer_or(ibuffer_size_gt(10000_int64), &
                                  ibuffer_and(ibuffer_not(ibuffer_starred_name()), ibuffer_directory("<org>")))
            test2(1) = ibuffer_or(ibuffer_major_mode("emacs_lisp_mode"), ibuffer_file_extension("elc?"), &
                                  ibuffer_and(ibuffer_starred_name(), ibuffer_name("elisp")), &
                                  ibuffer_major_mode("lisp_interaction_mode"))
            test3(1) = ibuffer_size_lt(100_int64)
            test3(2) = ibuffer_derived_mode("prog_mode")
            test3(3) = ibuffer_or(ibuffer_filename("scratch"), ibuffer_filename("bonz"), ibuffer_filename("temp"))
            call ibuffer_save_filters("test1", test1, st)
            call check(error, entry_is(1, "test1", test1), "test1 first")
            if (allocated(error)) return
            call ibuffer_save_filters("test2", test2, st)
            call check(error, entry_is(1, "test2", test2) .and. entry_is(2, "test1", test1), "test2 first")
            if (allocated(error)) return
            call ibuffer_save_filters("test3", test3, st)
            call check(error, entry_is(1, "test3", test3) .and. entry_is(2, "test2", test2) .and. &
                       entry_is(3, "test1", test1), "test3 first")
            if (allocated(error)) return
            call check(error, same_filters(ibuffer_saved_filters%entries(ibuffer_saved_filters%find("test2"))%filters, &
                                           test2), "assoc test2")
        end subroutine body
        logical function entry_is(k, name, filters)
            integer, intent(in) :: k
            character(len=*), intent(in) :: name
            type(ibuffer_filter_type), intent(in) :: filters(:)
            entry_is = .false.
            if (ibuffer_saved_filters%count() < k) return
            if (ibuffer_saved_filters%entries(k)%name /= name) return
            entry_is = same_filters(ibuffer_saved_filters%entries(k)%filters, filters)
        end function entry_is
    end subroutine test_save_filters

    !> Test for https://debbugs.gnu.org/25058 .
    subroutine test_bug25058(error)
        type(error_type), allocatable, intent(out) :: error
        type(ibuffer_saved_filter_groups_type) :: orig
        type(ibuffer_filter_groups_type) :: groups
        type(handle_type) :: old, ib
        type(status_type) :: st
        call get_test_ibuffer(old, ib, error)
        if (allocated(error)) return
        orig = ibuffer_saved_filter_groups
        allocate (groups%groups(0))
        call groups%add("Shell", ibuffer_used_mode("shell_mode"))
        call groups%add("Elisp", ibuffer_or(ibuffer_used_mode("emacs_lisp_mode"), &
                                            ibuffer_used_mode("lisp_interaction_mode")))
        call groups%add("Dired", ibuffer_used_mode("dired_mode"))
        call groups%add("Info", ibuffer_or(ibuffer_used_mode("help_mode"), ibuffer_used_mode("debugger_mode"), &
                                           ibuffer_used_mode("custom_mode"), &
                                           ibuffer_used_mode("completion_list_mode"), &
                                           ibuffer_name("\`[*]Messages[*]\'")))
        call ibuffer_saved_filter_groups%clear()
        call ibuffer_saved_filter_groups%add("saved-filters", groups)
        call ibuffer_switch_to_saved_filter_groups("saved-filters", st)
        call ibuffer_decompose_filter_group("Elisp", st)
        call ibuffer_filter_disable(st)
        call ibuffer_switch_to_saved_filter_groups("saved-filters", st)
        call check(error, ibuffer_saved_filter_groups%entries(1)%groups%find("Elisp") > 0, "Elisp group lost")
        ibuffer_saved_filter_groups = orig
        call kill_test_ibuffer(old, ib)
    end subroutine test_bug25058

    !> Tests inclusion using basic filter combinators with a single buffer.
    subroutine test_inclusion_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        call body()
        call clean_up()
    contains
        subroutine body()
            buf = create_file_buffer("ibuf-test-1", "", 100, "One ring to rule them all"//LF, FUNDAMENTAL)
            call should(error, incl(buf, ibuffer_size_gt(99_int64)), "size-gt")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_size_lt(101_int64)), "size-lt")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_major_mode("fundamental_mode")), "mode")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_content("ring to rule them all")), "content")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_and(ibuffer_content("ring to rule them all"))), "and")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_and(ibuffer_and(ibuffer_content("ring to rule them all")))), "and and")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_and(ibuffer_and(ibuffer_and(ibuffer_content("ring to rule them all"))))), &
                        "and and and")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_or(ibuffer_content("ring to rule them all"))), "or")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_not(ibuffer_not(ibuffer_content("ring to rule them all")))), "not not")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_and(ibuffer_size_gt(99_int64), ibuffer_content("ring to rule them all"), &
                                                     ibuffer_major_mode("fundamental_mode"), &
                                                     ibuffer_basename("\`ibuf-test-1"))), "and 4")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_not(ibuffer_or(ibuffer_not(ibuffer_size_gt(99_int64)), &
                                                                ibuffer_not(ibuffer_content("ring to rule them all")), &
                                                                ibuffer_not(ibuffer_major_mode("fundamental_mode")), &
                                                                ibuffer_not(ibuffer_basename("\`ibuf-test-1"))))), &
                        "not or 4")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_and(ibuffer_or(ibuffer_size_gt(99_int64), ibuffer_size_lt(10_int64)), &
                                                     ibuffer_and(ibuffer_content("ring.*all"), ibuffer_content("rule"), &
                                                                 ibuffer_content("them all"), ibuffer_content("One")), &
                                                     ibuffer_not(ibuffer_major_mode("text_mode")), &
                                                     ibuffer_basename("\`ibuf-test-1"))), "nested")
        end subroutine body
    end subroutine test_inclusion_1

    !> Tests inclusion of basic filters in combination on a single buffer.
    subroutine test_inclusion_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        call body()
        call clean_up()
    contains
        subroutine body()
            buf = create_file_buffer("ibuf-test-2", "", 200, "and in the darkness find them"//LF, TEXT)
            call should(error, incl(buf, ibuffer_size_gt(199_int64)), "size-gt")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_size_lt(201_int64)), "size-lt")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_not(ibuffer_size_gt(200_int64))), "not size-gt")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_and(ibuffer_size_gt(199_int64), ibuffer_size_lt(201_int64))), "and")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_or(ibuffer_size_gt(199_int64), ibuffer_size_gt(201_int64))), "or 1")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_or(ibuffer_size_gt(201_int64), ibuffer_size_gt(199_int64))), "or 2")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_size_gt(199_int64), ibuffer_major_mode("text_mode"), &
                                    ibuffer_content("darkness find them")), "list")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_and(ibuffer_size_gt(199_int64), ibuffer_major_mode("text_mode"), &
                                                     ibuffer_content("darkness find them"))), "and 3")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_not(ibuffer_or(ibuffer_not(ibuffer_size_gt(199_int64)), &
                                                                ibuffer_not(ibuffer_major_mode("text_mode")), &
                                                                ibuffer_not(ibuffer_content("darkness find them"))))), &
                        "not or")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_or(ibuffer_size_gt(200_int64), ibuffer_content("darkness find them"), &
                                                    ibuffer_derived_mode("emacs_lisp_mode"))), "or 3")
            if (allocated(error)) return
            call should(error,.not. incl(buf, ibuffer_or(ibuffer_size_gt(200_int64), ibuffer_content("rule them all"), &
                                                         ibuffer_derived_mode("emacs_lisp_mode"))), "not or 3")
        end subroutine body
    end subroutine test_inclusion_2

    !> Tests inclusion with filename filters on specified buffers.
    subroutine test_inclusion_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: bufa, bufb
        character(len=:), allocatable :: dira, dirb
        call body()
        call clean_up()
    contains
        subroutine body()
            bufa = create_file_buffer("ibuf-test-3.a", "", 50, "...but a multitude of drops?"//LF, TEXT)
            bufb = create_non_file_buffer("ibuf-test-3.b", 50, "...but a multitude of drops?"//LF, TEXT)
            dira = regexp_quote(directory_of(bufa))
            dirb = regexp_quote(directory_of(bufb))
            call should(error, incl(bufa, ibuffer_basename("ibuf-test-3")), "basename")
            if (allocated(error)) return
            call should(error, incl(bufa, ibuffer_basename("test-3\.a")), "basename 2")
            if (allocated(error)) return
            call should(error, incl(bufa, ibuffer_file_extension("a")), "file-extension")
            if (allocated(error)) return
            call should(error, incl(bufa, ibuffer_directory(dira)), "directory")
            if (allocated(error)) return
            call should(error,.not. incl(bufb, ibuffer_basename("ibuf-test-3")), "no basename")
            if (allocated(error)) return
            call should(error,.not. incl(bufb, ibuffer_file_extension("b")), "no file-extension")
            if (allocated(error)) return
            call should(error, incl(bufb, ibuffer_directory(dirb)), "default-directory")
            if (allocated(error)) return
            call should(error, incl(bufa, ibuffer_name("ibuf-test-3")), "name a")
            if (allocated(error)) return
            call should(error, incl(bufb, ibuffer_name("ibuf-test-3")), "name b")
        end subroutine body
    end subroutine test_inclusion_3

    !> Tests inclusion with various filters on a single buffer.
    subroutine test_inclusion_4(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf, old
        type(status_type) :: st
        call body()
        call clean_up()
    contains
        subroutine body()
            buf = create_file_buffer("ibuf-test-4", ".sh", 0, "echo --emacs-rocks--"//LF, SH)
            call should(error, incl(buf, ibuffer_file_extension("sh")), "file-extension")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_derived_mode("prog_mode")), "derived-mode")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_used_mode("sh_mode")), "used-mode")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_major_mode("sh_mode")), "mode")
            if (allocated(error)) return
            old = current_buffer()
            call set_buffer(buf, st)
            call restore_buffer_modified_p(.true.)
            call set_buffer(old, st)
            call should(error, incl(buf, ibuffer_modified()), "modified")
            if (allocated(error)) return
            call set_buffer(buf, st)
            call restore_buffer_modified_p(.false.)
            call set_buffer(old, st)
            call should(error, incl(buf, ibuffer_not(ibuffer_modified())), "not modified")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_and(ibuffer_file_extension("sh"), ibuffer_derived_mode("prog_mode"), &
                                                     ibuffer_not(ibuffer_modified()))), "and")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_or(ibuffer_file_extension("tex"), ibuffer_derived_mode("prog_mode"), &
                                                    ibuffer_modified())), "or")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_file_extension("sh"), ibuffer_derived_mode("prog_mode"), &
                                    ibuffer_not(ibuffer_modified())), "list")
        end subroutine body
    end subroutine test_inclusion_4

    !> Tests inclusion with various filters on a single buffer.
    subroutine test_inclusion_5(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        call body()
        call clean_up()
    contains
        subroutine body()
            buf = create_non_file_buffer("ibuf-test-5.el", 0, "echo --It really does!--"//LF, SH)
            call should(error,.not. incl(buf, ibuffer_file_extension("el")), "file-extension")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_size_gt(18_int64)), "size-gt")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_predicate(longer_than_18)), "predicate")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_and(ibuffer_major_mode("sh_mode"), &
                                                     ibuffer_or(ibuffer_starred_name(), ibuffer_size_gt(18_int64)), &
                                                     ibuffer_and(ibuffer_not(ibuffer_size_gt(100_int64)), &
                                                                 ibuffer_content("[Ii]t  *really does!"), &
                                                                 ibuffer_or(ibuffer_name("test-5"), &
                                                                            ibuffer_not(ibuffer_filename("test-5")))))), &
                        "nested")
        end subroutine body
    end subroutine test_inclusion_5

    !> Tests inclusion using saved filters and DeMorgan's laws.
    subroutine test_inclusion_6(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf, buf2
        call body()
        call clean_up()
    contains
        subroutine body()
            buf = create_non_file_buffer("*ibuf-test-6*", 65, "", TEXT)
            buf2 = create_file_buffer("ibuf-test-6a", ".sh", 0, "echo Hello, World!", SH)
            call should(error, incl(buf, ibuffer_starred_name()), "starred-name")
            if (allocated(error)) return
            call should(error,.not. incl(buf, ibuffer_saved("text document")), "saved text document")
            if (allocated(error)) return
            call should(error, incl(buf2, ibuffer_saved("programming")), "saved programming")
            if (allocated(error)) return
            call should(error, incl(buf2, ibuffer_not(ibuffer_and(ibuffer_not(ibuffer_derived_mode("prog_mode")), &
                                                                  ibuffer_not(ibuffer_derived_mode("conf_mode")), &
                                                                  ibuffer_not(ibuffer_major_mode("text_mode"))))), &
                        "DeMorgan")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_and(ibuffer_starred_name(), &
                                                     ibuffer_or(ibuffer_size_gt(50_int64), ibuffer_filename("foo")))), &
                        "and or")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_not(ibuffer_or(ibuffer_not(ibuffer_starred_name()), &
                                                                ibuffer_and(ibuffer_size_lt(51_int64), &
                                                                            ibuffer_not(ibuffer_filename("foo")))))), &
                        "not or")
        end subroutine body
    end subroutine test_inclusion_6

    !> Tests inclusion with various filters on a single buffer.
    subroutine test_inclusion_7(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        call body()
        call clean_up()
    contains
        subroutine body()
            buf = create_non_file_buffer("ibuf-test-7", 0, "", FUNDAMENTAL)
            call should(error, incl(buf, ibuffer_not(ibuffer_starred_name())), "not starred-name")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_not(ibuffer_not(ibuffer_not(ibuffer_starred_name())))), &
                        "not not not starred-name")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_not(ibuffer_modified())), "not modified")
            if (allocated(error)) return
            call should(error, incl(buf, ibuffer_not(ibuffer_not(ibuffer_not(ibuffer_modified())))), &
                        "not not not modified")
        end subroutine body
    end subroutine test_inclusion_7

    !> Tests inclusion with various filters.
    subroutine test_inclusion_8(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: bufa, bufb, bufc, bufd, bufe, buff, old
        type(status_type) :: st
        call body()
        call clean_up()
    contains
        subroutine body()
            bufa = create_non_file_buffer("ibuf-test-8a", 0, "", TEXT)
            bufb = create_non_file_buffer("*ibuf-test-8b*", 32, "", FUNDAMENTAL)
            bufc = create_file_buffer("ibuf-test8c", "*", 64, "", FUNDAMENTAL)
            bufd = create_file_buffer("*ibuf-test8d", "", 128, "", FUNDAMENTAL)
            bufe = create_file_buffer("*ibuf-test8e", "*<2>", 16, "", FUNDAMENTAL)
            buff = create_non_file_buffer("*ibuf-test8f*", 0, "", FUNDAMENTAL)
            buff = create_non_file_buffer("*ibuf-test8f*", 8, "", FUNDAMENTAL)
            old = current_buffer()
            call set_buffer(bufa, st)
            call restore_buffer_modified_p(.true.)
            call set_buffer(old, st)
            call should(error, incl(bufa, ibuffer_and(ibuffer_not(ibuffer_starred_name()), ibuffer_modified(), &
                                                      ibuffer_name("test-8"), ibuffer_not(ibuffer_size_gt(100_int64)), &
                                                      ibuffer_major_mode("text_mode"))), "A modified")
            if (allocated(error)) return
            call set_buffer(bufa, st)
            call restore_buffer_modified_p(.false.)
            call set_buffer(old, st)
            call should(error,.not. incl(bufa, ibuffer_or(ibuffer_starred_name(), ibuffer_visiting_file(), &
                                                          ibuffer_modified())), "A")
            if (allocated(error)) return
            call should(error, incl(bufb, ibuffer_and(ibuffer_starred_name(), ibuffer_name("test.*8b"), &
                                                      ibuffer_size_gt(31_int64), ibuffer_not(ibuffer_visiting_file()))), "B")
            if (allocated(error)) return
            call should(error, incl(bufc, ibuffer_and(ibuffer_not(ibuffer_starred_name()), ibuffer_visiting_file(), &
                                                      ibuffer_name("8c[^*]*\*"), ibuffer_size_lt(65_int64))), "C")
            if (allocated(error)) return
            call should(error, incl(bufd, ibuffer_and(ibuffer_not(ibuffer_starred_name()), ibuffer_visiting_file(), &
                                                      ibuffer_name("\`\*.*test8d"), ibuffer_size_lt(129_int64), &
                                                      ibuffer_size_gt(127_int64))), "D")
            if (allocated(error)) return
            call should(error, incl(bufe, ibuffer_and(ibuffer_starred_name(), ibuffer_visiting_file(), &
                                                      ibuffer_name("8e.*?\*<[[:digit:]]+>"), ibuffer_size_gt(10_int64))), &
                        "E")
            if (allocated(error)) return
            call should(error, incl(buff, ibuffer_and(ibuffer_starred_name(), ibuffer_not(ibuffer_visiting_file()), &
                                                      ibuffer_name("8f\*<[[:digit:]]>"), ibuffer_size_lt(10_int64))), "F")
        end subroutine body
    end subroutine test_inclusion_8

    !> Tests `ibuffer_decompose_filter' for and, or, not, and saved.
    subroutine test_decompose_filter(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, ib
        type(status_type) :: st
        type(ibuffer_filter_type) :: filters(3), gnus(1)
        type(ibuffer_filter_type), allocatable :: parts(:)
        integer :: k
        call body()
        call set_filters()
        call kill_test_ibuffer(old, ib)
    contains
        subroutine body()
            call get_test_ibuffer(old, ib, error)
            if (allocated(error)) return
            filters(1) = ibuffer_size_gt(100_int64)
            filters(2) = ibuffer_not(ibuffer_starred_name())
            filters(3) = ibuffer_name("foo")
            call set_filters(ibuffer_or(filters(1), filters(2), filters(3)))
            call ibuffer_decompose_filter(st)
            call check(error, same_filters(filters, ibuffer_filtering_qualifiers()), "or")
            if (allocated(error)) return
            call set_filters(ibuffer_and(filters(1), filters(2), filters(3)))
            call ibuffer_decompose_filter(st)
            call check(error, same_filters(filters, ibuffer_filtering_qualifiers()), "and")
            if (allocated(error)) return
            call set_filters(ibuffer_not(filters(1)))
            call ibuffer_decompose_filter(st)
            call check(error, same_filters(filters(1:1), ibuffer_filtering_qualifiers()), "not")
            if (allocated(error)) return
            k = ibuffer_saved_filters%find("gnus")
            gnus = ibuffer_saved_filters%entries(k)%filters
            call set_filters(ibuffer_saved("gnus"))
            call ibuffer_decompose_filter(st)
            call check(error, same_filters(gnus, ibuffer_filtering_qualifiers()), "saved")
            if (allocated(error)) return
            call ibuffer_decompose_filter(st)
            allocate (parts(5))
            parts(1) = ibuffer_major_mode("message_mode")
            parts(2) = ibuffer_major_mode("mail_mode")
            parts(3) = ibuffer_major_mode("gnus_group_mode")
            parts(4) = ibuffer_major_mode("gnus_summary_mode")
            parts(5) = ibuffer_major_mode("gnus_article_mode")
            call check(error, same_filters(parts, ibuffer_filtering_qualifiers()), "saved or")
            if (allocated(error)) return
            call set_filters(ibuffer_saved("__uknown__"))
            call ibuffer_decompose_filter(st)
            call check(error,.not. st%ok(), "unknown saved filter")
            if (allocated(error)) return
            call set_filters(filters(1))
            call ibuffer_decompose_filter(st)
            call check(error,.not. st%ok(), "not compound")
        end subroutine body
    end subroutine test_decompose_filter

    !> Tests `ibuffer_and_filter' in an Ibuffer buffer.
    subroutine test_and_filter(error)
        type(error_type), allocatable, intent(out) :: error
        call op_filter_test(error, .true.)
    end subroutine test_and_filter

    !> Tests `ibuffer_or_filter' in an Ibuffer buffer.
    subroutine test_or_filter(error)
        type(error_type), allocatable, intent(out) :: error
        call op_filter_test(error, .false.)
    end subroutine test_or_filter

    ! ibuffer-and-filter (AND) or ibuffer-or-filter.
    subroutine op_filter_test(error, and)
        type(error_type), allocatable, intent(out) :: error
        logical, intent(in) :: and
        type(handle_type) :: old, ib
        type(status_type) :: st
        type(ibuffer_filter_type) :: f(4), top(1), parts(2), outer(2)
        call body()
        call set_filters()
        call kill_test_ibuffer(old, ib)
    contains
        subroutine body()
            call get_test_ibuffer(old, ib, error)
            if (allocated(error)) return
            f(1) = ibuffer_size_gt(100_int64)
            f(2) = ibuffer_not(ibuffer_starred_name())
            f(3) = ibuffer_filename("A")
            f(4) = ibuffer_major_mode("text_mode")
            call set_filters()
            call run(.false.)
            call check(error,.not. st%ok(), "no filters")
            if (allocated(error)) return
            call set_filters(f(2))
            call run(.false.)
            call check(error,.not. st%ok(), "one filter")
            if (allocated(error)) return
            call set_filters(f(1), f(2))
            call run(.false.)
            top(1) = op(f(1), f(2))
            call check(error, same_filters(top, ibuffer_filtering_qualifiers()), "two")
            if (allocated(error)) return
            call run(.true.)
            call check(error, same_filters(f(1:2), ibuffer_filtering_qualifiers()), "decompose")
            if (allocated(error)) return
            ! Same op: flattened.
            call set_filters(op(f(1), f(2)), op(f(3), f(4)))
            call run(.false.)
            top(1) = op4(f(1), f(2), f(3), f(4))
            call check(error, same_filters(top, ibuffer_filtering_qualifiers()), "same op")
            if (allocated(error)) return
            ! The other op second.
            call set_filters(op(f(1), f(2)), other(f(3), f(4)))
            call run(.false.)
            top(1) = op3(f(1), f(2), other(f(3), f(4)))
            call check(error, same_filters(top, ibuffer_filtering_qualifiers()), "other second")
            if (allocated(error)) return
            ! The other op first.
            call set_filters(other(f(1), f(2)), op(f(3), f(4)))
            call run(.false.)
            top(1) = op3(other(f(1), f(2)), f(3), f(4))
            call check(error, same_filters(top, ibuffer_filtering_qualifiers()), "other first")
            if (allocated(error)) return
            ! Both the other op.
            call set_filters(other(f(1), f(2)), other(f(3), f(4)))
            call run(.false.)
            parts(1) = other(f(1), f(2))
            parts(2) = other(f(3), f(4))
            outer(1) = op(parts(1), parts(2))
            call check(error, same_filters(outer(1:1), ibuffer_filtering_qualifiers()), "both other")
        end subroutine body
        subroutine run(decompose)
            logical, intent(in) :: decompose
            if (and) then
                call ibuffer_and_filter(st, decompose)
            else
                call ibuffer_or_filter(st, decompose)
            end if
        end subroutine run
        function op(a, b) result(r)
            type(ibuffer_filter_type), intent(in) :: a, b
            type(ibuffer_filter_type) :: r
            if (and) then
                r = ibuffer_and(a, b)
            else
                r = ibuffer_or(a, b)
            end if
        end function op
        function other(a, b) result(r)
            type(ibuffer_filter_type), intent(in) :: a, b
            type(ibuffer_filter_type) :: r
            if (and) then
                r = ibuffer_or(a, b)
            else
                r = ibuffer_and(a, b)
            end if
        end function other
        function op3(a, b, c) result(r)
            type(ibuffer_filter_type), intent(in) :: a, b, c
            type(ibuffer_filter_type) :: r
            if (and) then
                r = ibuffer_and(a, b, c)
            else
                r = ibuffer_or(a, b, c)
            end if
        end function op3
        function op4(a, b, c, d) result(r)
            type(ibuffer_filter_type), intent(in) :: a, b, c, d
            type(ibuffer_filter_type) :: r
            if (and) then
                r = ibuffer_and(a, b, c, d)
            else
                r = ibuffer_or(a, b, c, d)
            end if
        end function op4
    end subroutine op_filter_test

    !> Tests string recommendation of filter from `ibuffer_format_qualifier'.
    subroutine test_format_qualifier(error)
        type(error_type), allocatable, intent(out) :: error
        call init_bindings()
        call expect(ibuffer_major_mode("org_mode"), " [major mode: org_mode]")
        if (allocated(error)) return
        call expect(ibuffer_size_lt(100_int64), " [size less than: 100]")
        if (allocated(error)) return
        call expect(ibuffer_derived_mode("prog_mode"), " [derived mode: prog_mode]")
        if (allocated(error)) return
        call expect(ibuffer_or(ibuffer_size_gt(10000_int64), &
                               ibuffer_and(ibuffer_not(ibuffer_starred_name()), ibuffer_directory("\<org\>"))), &
                    " [OR [size greater than: 10000] [AND [NOT [starred buffer name]] [directory name: \<org\>]]]")
        if (allocated(error)) return
        call expect(ibuffer_or(ibuffer_filename("scratch"), ibuffer_filename("bonz"), ibuffer_filename("temp")), &
                    " [OR [full file name: scratch] [full file name: bonz] [full file name: temp]]")
        if (allocated(error)) return
        call expect(ibuffer_or(ibuffer_major_mode("emacs_lisp_mode"), ibuffer_file_extension("elc?"), &
                               ibuffer_and(ibuffer_starred_name(), ibuffer_name("elisp")), &
                               ibuffer_major_mode("lisp_interaction_mode")), &
                    " [OR [major mode: emacs_lisp_mode] [filename extension: elc?] [AND [starred buffer name] " &
                    //"[buffer name: elisp]] [major mode: lisp_interaction_mode]]")
    contains
        subroutine expect(f, s)
            type(ibuffer_filter_type), intent(in) :: f
            character(len=*), intent(in) :: s
            character(len=:), allocatable :: got
            got = ibuffer_format_qualifier(f)
            call check(error, got == s, '"'//got//'" expected "'//s//'"')
        end subroutine expect
    end subroutine test_format_qualifier

    !> Tests `ibuffer_unary_operand': (not cell) -> cell.
    subroutine test_unary_operand(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, ibuffer_filter_equal(ibuffer_unary_operand(ibuffer_not(ibuffer_major_mode("foo"))), &
                                               ibuffer_major_mode("foo")), "not mode")
        if (allocated(error)) return
        call check(error, ibuffer_filter_equal(ibuffer_unary_operand(ibuffer_not(ibuffer_or(ibuffer_name("a"), &
                                                                                            ibuffer_name("b")))), &
                                               ibuffer_or(ibuffer_name("a"), ibuffer_name("b"))), "not or")
    end subroutine test_unary_operand

    !> Filter groups: making a group from the filters, the listing, killing
    !> and yanking groups, and filtering by name.
    subroutine test_filter_groups(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, ib, buf
        type(status_type) :: st
        type(ibuffer_filter_groups_type) :: groups
        character(len=:), allocatable :: listing
        type(ibuffer_filter_type), allocatable :: filters(:)
        call body()
        call kill_test_ibuffer(old, ib)
        call clean_up()
    contains
        subroutine body()
            buf = create_non_file_buffer("ibuf-group-test", 0, "", TEXT)
            call get_test_ibuffer(old, ib, error)
            if (allocated(error)) return
            call ibuffer_filter_by_name("ibuf-group-test", st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) return
            listing = buffer_string()
            call check(error, index(listing, "ibuf-group-test") > 0 .and. index(listing, "*Test-Ibuffer*") == 0, listing)
            if (allocated(error)) return
            call ibuffer_filters_to_filter_group("Tests", st)
            filters = ibuffer_filtering_qualifiers()
            call check(error, st%ok() .and. size(filters) == 0, "filters to group")
            if (allocated(error)) return
            groups = ibuffer_filter_groups()
            call check(error, groups%count() == 1, "one group")
            if (allocated(error)) return
            listing = buffer_string()
            call check(error, index(listing, "[ Tests ]") > 0 .and. index(listing, "[ Default ]") > 0 .and. &
                       index(listing, "[ Tests ]") < index(listing, "[ Default ]"), listing)
            if (allocated(error)) return
            call ibuffer_kill_filter_group("Tests", st)
            groups = ibuffer_filter_groups()
            call check(error, st%ok() .and. groups%count() == 0, "kill group")
            if (allocated(error)) return
            call ibuffer_kill_filter_group("Default", st)
            call check(error,.not. st%ok(), "kill Default")
            if (allocated(error)) return
            call ibuffer_yank_filter_group("Default", st)
            groups = ibuffer_filter_groups()
            call check(error, st%ok() .and. groups%count() == 1, "yank group")
            if (allocated(error)) return
            call ibuffer_yank_filter_group("Default", st)
            call check(error,.not. st%ok(), "kill ring empty")
            if (allocated(error)) return
            call ibuffer_pop_filter(st)
            call check(error,.not. st%ok(), "no filters to pop")
        end subroutine body
    end subroutine test_filter_groups

    !> `completing_read_multiple' completes each element and splits the
    !> input at the separators.
    subroutine test_crm(error)
        type(error_type), allocatable, intent(out) :: error
        type(completion_table_type) :: table
        type(minibuffer_string_type), allocatable :: results(:)
        type(value_type), allocatable :: evs(:)
        type(value_type) :: items(3)
        type(status_type) :: st
        call init_bindings()
        call discard_input()
        items(1) = make_string("foo")
        items(2) = make_string("bar")
        items(3) = make_string("qux")
        table%kind = TABLE_LIST
        table%list = make_list(items)
        call key_parse("f o TAB , b TAB RET", evs, st)
        call unread_command_events(evs)
        call completing_read_multiple("Things: ", table, results, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, size(results) == 2, "two elements")
        if (allocated(error)) return
        call check(error, results(1)%s == "foo" .and. results(2)%s == "bar", results(1)%s//"|"//results(2)%s)
    end subroutine test_crm
end module test_ibuf_ext_fm
