! test_info_fm.f90 -- tests for info_fm.
! Ported from GNU Emacs 31.1 test/lisp/info-tests.el.
!
! Copyright (C) 2023-2026 Free Software Foundation, Inc.
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

!> Ports test-info-urls of info-tests.el; the other tests are our own,
!> on the Info files of test/info-resources: test.info, the split file
!> split.info (split.info-1 and split.info-2) and the dir files of dir1
!> and dir2.
module test_info_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, list, make_string, nilp, stringp, xstring, symbolp, symbols, xsymbol, nil
    use lread_fm, only: sym
    use textprop_fm, only: get_text_property, get_char_property
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, kill_buffer, get_buffer, header_line_format_type
    use editfns_fm, only: buffer_string, point, point_min, point_max, goto_char, line_beginning_position
    use search_fm, only: search_forward, SEARCH_NOERROR
    use fileio_fm, only: expand_file_name, emacs_wd, init_fileio, file_name_as_directory, file_exists_p, &
                         file_name_directory, file_name_nondirectory
    use files_fm, only: make_temp_file, delete_file
    use callproc_fm, only: call_process
    use simple_fm, only: current_kill
    use window_fm, only: window_configuration_type, current_window_configuration, set_window_configuration
    use bindings_fm, only: init_bindings
    use info_fm, only: info_url_for_node, info_find_node, info_goto_node, info_current_file, info_current_node, &
                       info_current_subfile, info_next, info_prev, info_up, info_history_back, info_history_forward, &
                       info_menu, info_nth_menu_item, info_follow_reference, info_forward_node, info_backward_node, &
                       info_top_node, info_final_node, info_directory, info_index_node, info_index_nodes, &
                       info_copy_current_node_name, info_directory_list, info_next_reference, info_prev_reference, &
                       info_follow_nearest_node, info_extract_pointer, info_build_node_completions, info_toc, &
                       info_history, info_search, info_search_backward, info_index, info_index_next, &
                       info_virtual_index, info_history_variable, info_history_entry_type
    use isearch_fm, only: isearch_mode, isearch_process_search_string, isearch_repeat_forward, isearch_delete_char, &
                          isearch_done, isearch_success, isearch_error
    implicit none(type, external)
    private

    public :: collect_info_fm

    type :: string_type
        character(len=:), allocatable :: s
    end type string_type

contains

    subroutine collect_info_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("test-info-urls", test_info_urls), &
                    new_unittest("info-find-node", test_find_node), &
                    new_unittest("info-next-prev-up", test_next_prev_up), &
                    new_unittest("info-history", test_history), &
                    new_unittest("info-menu", test_menu), &
                    new_unittest("info-follow-reference", test_follow_reference), &
                    new_unittest("info-anchor", test_anchor), &
                    new_unittest("info-split-file", test_split_file), &
                    new_unittest("info-forward-backward-node", test_forward_backward_node), &
                    new_unittest("info-final-node", test_final_node), &
                    new_unittest("info-index-node", test_index_node), &
                    new_unittest("info-references", test_references), &
                    new_unittest("info-copy-current-node-name", test_copy_current_node_name), &
                    new_unittest("info-node-completions", test_node_completions), &
                    new_unittest("info-directory", test_directory), &
                    new_unittest("info-compressed-file", test_compressed_file), &
                    new_unittest("info-fontify-node", test_fontify_node), &
                    new_unittest("info-breadcrumbs", test_breadcrumbs), &
                    new_unittest("info-toc", test_toc), &
                    new_unittest("info-history-node", test_history_node), &
                    new_unittest("info-search", test_search), &
                    new_unittest("info-search-split", test_search_split), &
                    new_unittest("info-index", test_index), &
                    new_unittest("info-virtual-index", test_virtual_index), &
                    new_unittest("info-isearch", test_isearch) &
                    ]
    end subroutine collect_info_fm

    ! ------------------------------------------------------------------
    ! The environment of the tests
    ! ------------------------------------------------------------------

    !> The directory of the resources, or its subdirectory SUB.
    function resource_dir(sub) result(dir)
        character(len=*), intent(in), optional :: sub
        character(len=:), allocatable :: dir
        type(status_type) :: st
        call init_fileio()
        call expand_file_name("test/info-resources", dir, st, emacs_wd)
        dir = file_name_as_directory(dir)
        if (present(sub)) dir = file_name_as_directory(dir//sub)
    end function resource_dir

    !> Check that FILE is the resource file NAME.
    subroutine check_file(error, file, name)
        type(error_type), allocatable, intent(inout) :: error
        character(len=*), intent(in) :: file, name
        character(len=:), allocatable :: expected
        expected = resource_dir()
        expected = expected//name
        call check(error, file, expected)
    end subroutine check_file

    !> Search Info files in the resource directory only.
    subroutine use_resources()
        character(len=:), allocatable :: dir
        call init_bindings()
        dir = resource_dir()
        call set_directories([dir])
    end subroutine use_resources

    !> Set `info_directory_list' to DIRS.
    subroutine set_directories(dirs)
        character(len=*), intent(in) :: dirs(:)
        integer :: k
        if (allocated(info_directory_list)) deallocate (info_directory_list)
        allocate (character(len=len(dirs)) :: info_directory_list(size(dirs)))
        do k = 1, size(dirs)
            info_directory_list(k) = dirs(k)
        end do
    end subroutine set_directories

    !> Visit node NODE of FILE in a fresh *info* buffer, saving the window
    !> configuration in CONFIG and the buffer current before in OLD.
    subroutine visit(file, node, old, config, error)
        character(len=*), intent(in) :: file, node
        type(handle_type), intent(out) :: old
        type(window_configuration_type), intent(out) :: config
        type(error_type), allocatable, intent(inout) :: error
        type(handle_type) :: info_buffer
        type(status_type) :: st
        call use_resources()
        old = current_buffer()
        call current_window_configuration(config)
        info_buffer = get_buffer("*info*")
        if (info_buffer%index /= 0) call kill_buffer(info_buffer, st)
        call info_find_node(file, node, st)
        call check(error, st%ok(), "visit: "//format_error(st))
    end subroutine visit

    !> The end of a test: kill *info* and go back to OLD.
    subroutine done(old, config)
        type(handle_type), intent(in) :: old
        type(window_configuration_type), intent(in) :: config
        type(handle_type) :: info_buffer
        type(status_type) :: st
        call set_window_configuration(config)
        info_buffer = get_buffer("*info*")
        if (info_buffer%index /= 0) call kill_buffer(info_buffer, st)
        call set_buffer(old, st)
    end subroutine done

    !> The current node's name, "" for none.
    function node() result(n)
        character(len=:), allocatable :: n
        n = info_current_node()
        if (.not. allocated(n)) n = ""
    end function node

    !> Check that the current node is EXPECTED.
    subroutine check_node(error, expected, st)
        type(error_type), allocatable, intent(inout) :: error
        character(len=*), intent(in) :: expected
        type(status_type), intent(in) :: st
        if (allocated(error)) return
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, node(), expected)
    end subroutine check_node

    !> The text of the current line.
    function current_line() result(text)
        character(len=:), allocatable :: text
        character(len=:), allocatable :: all
        integer(int64) :: b, e, i
        all = buffer_string()
        b = line_beginning_position(1_int64) - point_min() + 1
        e = b
        do i = b, len(all)
            if (all(i:i) == achar(10)) exit
            e = i
        end do
        text = all(b:e)
    end function current_line

    ! ------------------------------------------------------------------
    ! The tests
    ! ------------------------------------------------------------------

    subroutine test_info_urls(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: url
        call check(error, info_url_for_node("(tramp)Top", st), &
                   "https://www.gnu.org/software/emacs/manual/html_node/tramp/")
        if (allocated(error)) return
        call check(error, info_url_for_node("(emacs)Minibuffer", st), &
                   "https://www.gnu.org/software/emacs/manual/html_node/emacs/Minibuffer.html")
        if (allocated(error)) return
        call check(error, info_url_for_node("(emacs)Minibuffer File", st), &
                   "https://www.gnu.org/software/emacs/manual/html_node/emacs/Minibuffer-File.html")
        if (allocated(error)) return
        call check(error, info_url_for_node("(elisp)Backups and Auto-Saving", st), &
                   "https://www.gnu.org/software/emacs/manual/html_node/elisp/Backups-and-Auto_002dSaving.html")
        if (allocated(error)) return
        call check(error, info_url_for_node("(eintr)car & cdr", st), &
                   "https://www.gnu.org/software/emacs/manual/html_node/eintr/car-_0026-cdr.html")
        if (allocated(error)) return
        call check(error, info_url_for_node("(emacs-mime)"//achar(9)//"Index", st), &
                   "https://www.gnu.org/software/emacs/manual/html_node/emacs-mime/Index.html")
        if (allocated(error)) return
        call check(error, info_url_for_node("(gnus) Don't Panic", st), &
                   "https://www.gnu.org/software/emacs/manual/html_node/gnus/Don_0027t-Panic.html")
        if (allocated(error)) return
        url = info_url_for_node("(nonexistent)Example", st)
        call check(error,.not. st%ok(), "should error")
    end subroutine test_info_urls

    subroutine test_find_node(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(header_line_format_type) :: id
        character(len=:), allocatable :: file
        call visit("test", "Top", old, config, error)
        if (.not. allocated(error)) call check(error, node(), "Top")
        if (.not. allocated(error)) then
            file = info_current_file()
            call check(error, allocated(file), "a current file")
        end if
        if (.not. allocated(error)) call check_file(error, file, "test")
        if (.not. allocated(error)) call check(error, buffers(cur)%name, "*info*")
        if (.not. allocated(error)) call check(error, buffers(cur)%major_mode, "info_mode")
        ! The buffer is narrowed to the node, point after its header line.
        if (.not. allocated(error)) call check(error, index(buffer_string(), "Node: First Chapter") == 0, &
                                               "narrowed to the node")
        if (.not. allocated(error)) call check(error, line_beginning_position(1_int64) /= point_min(), &
                                               "point below the header line")
        ! The mode line shows the manual and the node.
        if (.not. allocated(error)) then
            id = buffers(cur)%mode_line_buffer_identification
            call check(error, size(id%segments), 3)
        end if
        if (.not. allocated(error)) call check(error, id%segments(2)%text, " (test) ")
        if (.not. allocated(error)) call check(error, id%segments(3)%text, "Top")
        call done(old, config)
    end subroutine test_find_node

    subroutine test_next_prev_up(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        call visit("test", "Top", old, config, error)
        call info_next(st)
        call check_node(error, "First Chapter", st)
        call info_next(st)
        call check_node(error, "Second Chapter", st)
        call info_prev(st)
        call check_node(error, "First Chapter", st)
        call info_up(st)
        call check_node(error, "Top", st)
        ! Point goes to the menu entry of the node left.
        if (.not. allocated(error)) call check(error, current_line(), "* First Chapter::       The first chapter.")
        if (.not. allocated(error)) then
            call info_up(st)
            call check(error,.not. st%ok(), "Top's Up is (dir), not in the resources")
        end if
        if (.not. allocated(error)) then
            call info_goto_node("Section One", st)
            call info_next(st)
            call check(error,.not. st%ok(), "Section One has no Next")
        end if
        if (.not. allocated(error)) call check(error, format_error(st), "Node has no Next")
        call done(old, config)
    end subroutine test_next_prev_up

    subroutine test_history(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        call visit("test", "Top", old, config, error)
        call info_goto_node("Second Chapter", st)
        call check_node(error, "Second Chapter", st)
        call info_history_back(st)
        call check_node(error, "Top", st)
        call info_history_forward(st)
        call check_node(error, "Second Chapter", st)
        if (.not. allocated(error)) then
            call info_history_forward(st)
            call check(error, format_error(st), "This is the last Info node you looked at")
        end if
        call info_history_back(st)
        call check_node(error, "Top", st)
        ! (Visiting the file from another buffer recorded Top as the node
        ! left, as upstream: `info_pop_to_buffer' had already shown it.)
        call info_history_back(st)
        call check_node(error, "Top", st)
        if (.not. allocated(error)) then
            call info_history_back(st)
            call check(error, format_error(st), "This is the first Info node you looked at")
        end if
        call done(old, config)
    end subroutine test_history

    subroutine test_menu(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        call visit("test", "Top", old, config, error)
        call info_menu("Second", st)
        call check_node(error, "Second Chapter", st)
        call info_top_node(st)
        call check_node(error, "Top", st)
        call info_nth_menu_item(3, st)
        call check_node(error, "Index", st)
        call info_top_node(st)
        if (.not. allocated(error)) then
            call info_nth_menu_item(4, st)
            call check(error, format_error(st), "Too few items in menu")
        end if
        if (.not. allocated(error)) then
            call info_menu("Nothing", st)
            call check(error, format_error(st), "No such item in menu")
        end if
        call done(old, config)
    end subroutine test_menu

    subroutine test_follow_reference(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        call visit("test", "Second Chapter", old, config, error)
        ! The reference's node name spans two lines.
        call info_follow_reference("the first", st)
        call check_node(error, "First Chapter", st)
        call info_follow_reference("Section One", st)
        call check_node(error, "Section One", st)
        if (.not. allocated(error)) then
            call info_follow_reference("nowhere", st)
            call check(error, format_error(st), "No cross-reference named nowhere")
        end if
        call done(old, config)
    end subroutine test_follow_reference

    subroutine test_anchor(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        call visit("test", "an anchor", old, config, error)
        call check_node(error, "Section One", st)
        if (.not. allocated(error)) call check(error, current_line(), "An anchor is here.")
        call done(old, config)
    end subroutine test_anchor

    subroutine test_split_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        character(len=:), allocatable :: subfile
        call visit("split", "Second Chapter", old, config, error)
        call check_node(error, "Second Chapter", st)
        if (.not. allocated(error)) then
            subfile = info_current_subfile()
            call check(error, allocated(subfile), "a subfile")
        end if
        if (.not. allocated(error)) call check(error, subfile, "split.info-2")
        call info_goto_node("Top", st)
        call check_node(error, "Top", st)
        if (.not. allocated(error)) call check(error, info_current_subfile(), "split.info-1")
        call info_goto_node("an anchor", st)
        call check_node(error, "Section One", st)
        if (.not. allocated(error)) call check(error, current_line(), "An anchor is here.")
        call info_goto_node("(test)Index", st)
        call check_node(error, "Index", st)
        if (.not. allocated(error)) call check_file(error, info_current_file(), "test")
        call done(old, config)
    end subroutine test_split_file

    subroutine test_forward_backward_node(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        call visit("test", "Top", old, config, error)
        call info_forward_node(st)
        call check_node(error, "First Chapter", st)
        call info_forward_node(st)
        call check_node(error, "Section One", st)
        call info_forward_node(st)
        call check_node(error, "Second Chapter", st)
        call info_forward_node(st)
        call check_node(error, "Index", st)
        call info_backward_node(st)
        call check_node(error, "Second Chapter", st)
        call info_backward_node(st)
        call check_node(error, "Section One", st)
        call info_backward_node(st)
        call check_node(error, "First Chapter", st)
        call done(old, config)
    end subroutine test_forward_backward_node

    subroutine test_final_node(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        call visit("test", "First Chapter", old, config, error)
        call info_final_node(st)
        call check_node(error, "Index", st)
        call done(old, config)
    end subroutine test_final_node

    subroutine test_index_node(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        call visit("test", "Index", old, config, error)
        if (.not. allocated(error)) call check(error, info_index_node(), "Index has the index cookie")
        call info_goto_node("Top", st)
        if (.not. allocated(error)) call check(error,.not. info_index_node(), "Top is not an index")
        if (.not. allocated(error)) call check(error, info_index_node("Index"), "Index is an index node")
        ! An index entry goes to its line in the node.
        call info_goto_node("Index", st)
        call info_menu("section", st)
        call check_node(error, "Section One", st)
        if (.not. allocated(error)) call check(error, current_line(), "Some text.")
        call done(old, config)
    end subroutine test_index_node

    subroutine test_references(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        call visit("test", "Top", old, config, error)
        call goto_char(point_min())
        call info_next_reference(st)
        if (.not. allocated(error)) call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, index(current_line(), "*note Second Chapter::") > 0, &
                                               "on the cross reference")
        call info_next_reference(st)
        if (.not. allocated(error)) call check(error, current_line(), "* First Chapter::       The first chapter.")
        call info_prev_reference(st)
        if (.not. allocated(error)) call check(error, index(current_line(), "*note Second Chapter::") > 0, &
                                               "back on the cross reference")
        ! RET on the reference follows it.
        call info_follow_nearest_node(st)
        call check_node(error, "Second Chapter", st)
        call done(old, config)
    end subroutine test_references

    subroutine test_copy_current_node_name(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: kill
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        call visit("test", "First Chapter", old, config, error)
        call info_copy_current_node_name(st)
        call current_kill(0_int64, kill, st)
        if (.not. allocated(error)) call check(error, kill, "(test) First Chapter")
        call info_copy_current_node_name(st, 0_int64)
        call current_kill(0_int64, kill, st)
        if (.not. allocated(error)) call check(error, kill, "(info ""(test) First Chapter"")")
        call done(old, config)
    end subroutine test_copy_current_node_name

    subroutine test_node_completions(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        character(len=:), allocatable :: value
        call visit("test", "Top", old, config, error)
        call info_extract_pointer("next", value, st)
        if (.not. allocated(error)) call check(error, value, "First Chapter")
        call info_extract_pointer("prev", value, st, noerror=.true.)
        if (.not. allocated(error)) call check(error,.not. allocated(value), "no Prev")
        call done(old, config)
    end subroutine test_node_completions

    subroutine test_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        character(len=:), allocatable :: text, dir0, dir1, dir2
        integer :: n
        call visit("test", "Top", old, config, error)
        dir1 = resource_dir("dir1")
        dir2 = resource_dir("dir2")
        dir0 = resource_dir()
        call set_directories([character(len=len(dir1)) :: dir1, dir2, dir0])
        call info_directory(st)
        call check_node(error, "Top", st)
        if (.not. allocated(error)) then
            text = buffer_string()
            call check(error, count_of(text, "* Test: (test).") == 1, "one Test entry:"//text)
        end if
        if (.not. allocated(error)) call check(error, count_of(text, "* Split: (split).") == 1, "one Split entry")
        if (.not. allocated(error)) call check(error, count_of(text, achar(10)//"Testing"//achar(10)) == 1, &
                                               "the Testing headings merged:"//text)
        if (.not. allocated(error)) call check(error, count_of(text, "* Other: (other).") == 1, "the Other entry")
        if (.not. allocated(error)) then
            n = index(text, "This (the Directory node)")
            call check(error, n > 0, "the text of the first dir file")
        end if
        ! A menu item of the directory goes to the manual.
        if (.not. allocated(error)) then
            call info_menu("Split", st)
            call check_node(error, "Top", st)
        end if
        if (.not. allocated(error)) call check_file(error, info_current_file(), "split")
        call done(old, config)
        call use_resources()
    contains

        !> The number of occurrences of S in T.
        integer function count_of(t, s) result(c)
            character(len=*), intent(in) :: t, s
            integer :: i, j
            c = 0
            i = 1
            do
                j = index(t(i:), s)
                if (j == 0) exit
                c = c + 1
                i = i + j
            end do
        end function count_of
    end subroutine test_directory

    subroutine test_compressed_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, info_buffer
        type(window_configuration_type) :: config
        type(status_type) :: st
        type(value_type) :: result
        character(len=:), allocatable :: temp, zipped, command
        call make_temp_file("info-test", temp, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        ! (The empty temporary file itself would be read as the Info file,
        ! with the empty suffix.)
        call delete_file(temp, st)
        zipped = temp//".info.gz"
        command = resource_dir()
        command = "gzip -c "//command//"test.info > "//zipped
        call call_process("sh", result, st, args=list(make_string("-c"), make_string(command)))
        if (.not. file_exists_p(zipped)) then
            ! (No gzip: nothing to test.)
            return
        end if
        call use_resources()
        command = file_name_directory(temp)
        call set_directories([command])
        old = current_buffer()
        call current_window_configuration(config)
        info_buffer = get_buffer("*info*")
        if (info_buffer%index /= 0) call kill_buffer(info_buffer, st)
        call info_find_node(file_name_nondirectory(temp), "Second Chapter", st)
        call check_node(error, "Second Chapter", st)
        call done(old, config)
        call delete_file(zipped, st)
        call use_resources()
    end subroutine test_compressed_file

    !> The text property PROP at the first occurrence of TEXT in the node
    !> (FOUND false if TEXT is absent).
    subroutine property_at(text, prop, val, found)
        character(len=*), intent(in) :: text, prop
        type(value_type), intent(out) :: val
        logical, intent(out) :: found
        type(status_type) :: st
        integer(int64) :: pos
        call goto_char(point_min())
        call search_forward(text, pos, st, noerror=SEARCH_NOERROR)
        found = pos > 0
        if (.not. found) return
        call get_text_property(pos - len(text), sym(prop), val, st)
    end subroutine property_at

    !> The name of the symbol V, "" if V is not a symbol.
    function symbol_text(v) result(name)
        type(value_type), intent(in) :: v
        character(len=:), allocatable :: name
        name = ""
        if (symbolp(v)) name = trim(symbols(xsymbol(v))%name)
    end function symbol_text

    subroutine test_fontify_node(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        type(value_type) :: val
        logical :: found
        call visit("test", "Top", old, config, error)
        ! The header line shows the node's pointers.
        if (.not. allocated(error)) call check(error, allocated(buffers(cur)%header_line_format%segments), &
                                               "a header line")
        if (.not. allocated(error)) call check(error, buffers(cur)%header_line_format%segments(1)%text, "Next: ")
        ! "See *note" shows as "See" ("*note" displays as nothing).
        if (.not. allocated(error)) then
            call property_at("*note Second", "display", val, found)
            call check(error, found .and. stringp(val), "a display string on *note")
        end if
        if (.not. allocated(error)) call check(error, xstring(val), "")
        ! "*Note" at the start of a sentence shows as "See".
        call info_goto_node("First Chapter", st)
        if (.not. allocated(error)) then
            call property_at("*Note Section", "display", val, found)
            call check(error, found .and. stringp(val), "a display string on *Note")
        end if
        if (.not. allocated(error)) call check(error, xstring(val), "See ")
        call info_goto_node("Top", st)
        ! The reference is a link (visited if an earlier test went there,
        ! `info_history_list' being global).
        if (.not. allocated(error)) then
            call property_at("Second Chapter:: for", "font_lock_face", val, found)
            call check(error, symbol_text(val) == "info_xref" .or. symbol_text(val) == "info_xref_visited", &
                       "a link: "//symbol_text(val))
        end if
        ! The third menu item's star is highlighted.
        if (.not. allocated(error)) then
            call property_at("* Index::", "font_lock_face", val, found)
            call check(error, symbol_text(val), "info_menu_star")
        end if
        ! The menu header.
        if (.not. allocated(error)) then
            call property_at("* Menu:", "font_lock_face", val, found)
            call check(error, symbol_text(val), "info_menu_header")
        end if
        ! An index node's line numbers are hidden.
        call info_goto_node("Index", st)
        if (.not. allocated(error)) then
            call property_at(" (line 6)", "invisible", val, found)
            call check(error, found .and. .not. nilp(val), "the line number hidden")
        end if
        call done(old, config)
    end subroutine test_fontify_node

    subroutine test_breadcrumbs(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(value_type) :: val
        call visit("test", "Section One", old, config, error)
        ! The first line is replaced by the breadcrumbs.
        if (.not. allocated(error)) then
            val = get_char_property(point_min(), sym("display"))
            call check(error, stringp(val), "breadcrumbs on the first line")
        end if
        if (.not. allocated(error)) call check(error, xstring(val), "(test)Top > First Chapter > Section One"//achar(10))
        call done(old, config)
    end subroutine test_breadcrumbs

    subroutine test_toc(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        character(len=:), allocatable :: text
        call visit("test", "First Chapter", old, config, error)
        call info_toc(st)
        call check_node(error, "*TOC*", st)
        if (.not. allocated(error)) then
            text = buffer_string()
            call check(error, index(text, "*Note First Chapter:: ") > 0, "First Chapter listed:"//text)
        end if
        if (.not. allocated(error)) call check(error, index(text, achar(9)//"*Note Section One:: ") > 0, &
                                               "Section One indented")
        ! Point is on the node left.
        if (.not. allocated(error)) call check(error, index(current_line(), "First Chapter") > 0, &
                                               "point on First Chapter")
        call done(old, config)
    end subroutine test_toc

    subroutine test_history_node(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        character(len=:), allocatable :: text
        call visit("test", "First Chapter", old, config, error)
        call info_goto_node("Second Chapter", st)
        call info_history(st)
        call check_node(error, "Top", st)
        if (.not. allocated(error)) call check(error, info_current_file(), "*History*")
        if (.not. allocated(error)) then
            text = buffer_string()
            call check(error, index(text, "* Second Chapter: (") > 0, "Second Chapter listed:"//text)
        end if
        call done(old, config)
    end subroutine test_history_node

    subroutine test_search(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        call visit("test", "Top", old, config, error)
        ! A match in another node selects it.
        call info_search("Some text", st)
        call check_node(error, "Section One", st)
        if (.not. allocated(error)) call check(error, index(current_line(), "Some text") > 0, "on the match")
        ! Backward.
        call info_search_backward("top node", st)
        call check_node(error, "Top", st)
        if (.not. allocated(error)) then
            call info_search("no such text anywhere", st)
            call check(error, format_error(st), "Search failed: ""no such text anywhere""")
        end if
        call check_node(error, "Top", status_type())
        call done(old, config)
    end subroutine test_search

    subroutine test_search_split(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        call visit("split", "Top", old, config, error)
        ! The match is in the other subfile.
        call info_search("Second chapter text", st)
        call check_node(error, "Second Chapter", st)
        if (.not. allocated(error)) call check(error, info_current_subfile(), "split.info-2")
        call done(old, config)
    end subroutine test_search_split

    subroutine test_index(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        call visit("test", "Top", old, config, error)
        call info_index("section", st)
        call check_node(error, "Section One", st)
        if (.not. allocated(error)) call check(error, current_line(), "Some text.")
        ! "chapter" matches one entry.
        call info_index("chapter", st)
        call check_node(error, "First Chapter", st)
        if (.not. allocated(error)) then
            call info_index_next(1_int64, st)
            call check_node(error, "First Chapter", st)
        end if
        if (.not. allocated(error)) then
            call info_index("nothing", st)
            call check(error, format_error(st), "No `nothing' in index")
        end if
        ! An empty topic goes to the index.
        call info_index("", st)
        call check_node(error, "Index", st)
        call done(old, config)
    end subroutine test_index

    subroutine test_virtual_index(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        character(len=:), allocatable :: text
        call visit("test", "Top", old, config, error)
        call info_virtual_index("e", st)
        call check_node(error, "*Index for ‘e’*", st)
        if (.not. allocated(error)) then
            text = buffer_string()
            call check(error, index(text, "* first chapter [Index]:") > 0, "the first entry:"//text)
        end if
        if (.not. allocated(error)) call check(error, index(text, "* section [Index]:") > 0, "the second entry")
        call done(old, config)
    end subroutine test_virtual_index

    subroutine test_isearch(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(info_history_entry_type), allocatable :: history(:)
        integer :: h0
        call visit("test", "Top", old, config, error)
        call info_history_variable(history)
        h0 = size(history)
        call isearch_mode(.true.)
        call isearch_process_search_string("Some text", "Some text")
        ! Isearch fails once at the end of the initial node.
        call check(error,.not. isearch_success, "fails in the initial node")
        if (.not. allocated(error)) call check(error, allocated(isearch_error), "an error")
        if (.not. allocated(error)) call check(error, isearch_error, "end of node")
        call check_node(error, "Top", status_type())
        ! C-s goes on through the other nodes.
        if (.not. allocated(error)) then
            call isearch_repeat_forward(nil())
            call check(error, isearch_success, "found in another node")
        end if
        call check_node(error, "Section One", status_type())
        ! DEL goes back to the initial node with the state it restores.
        if (.not. allocated(error)) then
            call isearch_delete_char()
            call check_node(error, "Top", status_type())
        end if
        if (.not. allocated(error)) then
            call isearch_repeat_forward(nil())
            call check_node(error, "Section One", status_type())
        end if
        call isearch_done()
        ! Only the node where Isearch ended is added to the history.
        if (.not. allocated(error)) then
            call info_history_variable(history)
            call check(error, size(history), h0 + 1)
        end if
        call done(old, config)
    end subroutine test_isearch
end module test_info_fm
