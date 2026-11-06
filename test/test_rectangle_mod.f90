! test_rectangle_mod.f90 -- tests for the rectangle commands (rectangle_mod).
! Own tests; upstream has no rect tests.
!
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

module test_rectangle_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use execute_interactively_mod, only: prefix_argument_type
    use value_mod, only: value_type
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error, condition_matches, BUFFER_READ_ONLY_CODE
    use handle_mod, only: handle_type
    use data_mod, only: set
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer, set_buffer_read_only
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: point, goto_character, buffer_string, mark_position, region_beginning, region_end, &
                                     interactive_region
    use keyboard_mod, only: keyboard_store_bytes, command_loop, discard_input, last_message
    use execute_interactively_mod, only: prefix_argument
    use bindings_mod, only: initialize_bindings
    use simple_mod, only: deactivate_mark, set_mark, cut_read_only_ok, count_lines, line_number_at_position, &
                          current_clipboard_entry, is_region_active, region_bounds, is_region_noncontiguous
    use modes_mod, only: minor_modes
    use indent_mod, only: indent_tabs_mode
    use rectangle_mod, only: last_cut_rectangle, extract_rectangle, delete_rectangle, &
                             delete_extract_rectangle, cut_rectangle, paste_rectangle, insert_rectangle, open_rectangle, &
                             clear_rectangle, string_rectangle, string_insert_rectangle, delete_whitespace_rectangle, &
                             rectangle_number_lines, spaces_string, extract_rectangle_bounds, rectangle_dimensions, &
                             rectangle_position_as_coordinates, is_rectangle_intersect
    use string_type_mod, only: string_type, append_string
    implicit none(type, external)
    private

    public :: collect_rectangle_mod

    character(len=1), parameter :: newline = achar(10), tab = achar(9)
    !> "abcdef\nghijkl\nmnopqr": the corners of the rectangle of columns 1
    !> to 4 are 2 (b) and 19 (q).
    character(len=*), parameter :: grid = "abcdef"//newline//"ghijkl"//newline//"mnopqr"

contains

    !> Collect all exported unit tests.
    subroutine collect_rectangle_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("extract-rectangle", test_extract), &
                    new_unittest("extract-rectangle-tabs", test_extract_tabs), &
                    new_unittest("delete-rectangle", test_delete), &
                    new_unittest("delete-rectangle-splits-tab", test_delete_tab), &
                    new_unittest("delete-rectangle-wide-character", test_delete_wide), &
                    new_unittest("delete-rectangle-fill", test_delete_fill), &
                    new_unittest("delete-extract-rectangle", test_delete_extract), &
                    new_unittest("kill-and-yank-rectangle", test_kill_paste), &
                    new_unittest("kill-rectangle-read-only", test_kill_read_only), &
                    new_unittest("insert-rectangle-extends-lines", test_insert_extends), &
                    new_unittest("open-rectangle", test_open), &
                    new_unittest("clear-rectangle", test_clear), &
                    new_unittest("string-rectangle", test_string), &
                    new_unittest("string-insert-rectangle", test_string_insert), &
                    new_unittest("delete-whitespace-rectangle", test_delete_whitespace), &
                    new_unittest("rectangle-number-lines", test_number_lines), &
                    new_unittest("rectangle-geometry", test_geometry), &
                    new_unittest("rectangle-keys", test_keys), &
                    new_unittest("rectangle-mark-mode-kill-yank", test_mark_mode_kill_paste), &
                    new_unittest("rectangle-mark-mode-past-eol", test_mark_mode_past_end_of_line), &
                    new_unittest("rectangle-mark-mode-region", test_mark_mode_region) &
                    ]
    end subroutine collect_rectangle_mod

    !> A fresh current buffer holding TEXT, point at its start.
    subroutine fresh(h, text)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: text
        type(status_type) :: st

        call get_or_make_buffer("*rect-test*", h, st)
        call set_buffer(h, st)
        call set_buffer_read_only(h, .false., st)
        call erase_buffer(st)
        call deactivate_mark(.true.)
        call insert(text, st)
        call goto_character(1_int64)
        indent_tabs_mode%value = .true.
        call show_alone()
    end subroutine fresh

    !> The lines of RECTANGLE joined by "|".
    function joined(rectangle) result(s)
        type(string_type), intent(in) :: rectangle(:)
        character(len=:), allocatable :: s
        integer :: k

        s = ""

        do k = 1, size(rectangle)
            if (k > 1) s = s//"|"
            s = s//rectangle(k)%text
        end do
    end function joined

    subroutine test_extract(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(string_type), allocatable :: rectangle(:)

        call fresh(h, grid)
        call goto_character(10_int64)
        call extract_rectangle(2_int64, 19_int64, rectangle, st)
        call check(error, joined(rectangle), "bcd|hij|nop")
        if (allocated(error)) return
        ! Point is kept; the start column may be the right one.
        call check(error, point(), 10_int64)
        if (allocated(error)) return
        call extract_rectangle(5_int64, 16_int64, rectangle, st)
        call check(error, joined(rectangle), "bcd|hij|nop")
        if (allocated(error)) return
        ! The lines are those from START's to END's, so START after END
        ! gives START's line only (as upstream's apply-on-rectangle).
        call extract_rectangle(19_int64, 2_int64, rectangle, st)
        call check(error, joined(rectangle), "nop")
        if (allocated(error)) return
        ! Lines too short for the rectangle are padded with spaces.
        call fresh(h, "abcdef"//newline//"g"//newline//"mnopqr")
        call extract_rectangle(2_int64, 14_int64, rectangle, st)
        call check(error, joined(rectangle), "bcd|   |nop")
        call close_buffer(h, st)
    end subroutine test_extract

    subroutine test_extract_tabs(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(string_type), allocatable :: rectangle(:)

        ! A TAB inside the rectangle becomes the spaces it takes.
        call fresh(h, "a"//tab//"b"//newline//"0123456789")
        call extract_rectangle(1_int64, 14_int64, rectangle, st)
        call check(error, joined(rectangle), "a       b|012345678")
        if (allocated(error)) return
        ! A TAB across the start column: the rectangle line starts after
        ! it, with the columns from the start column (as upstream).
        call fresh(h, "0123456789"//newline//"a"//tab//"b"//newline//"0123456789")
        call extract_rectangle(3_int64, 21_int64, rectangle, st)
        call check(error, joined(rectangle), "234|      |234")
        call close_buffer(h, st)
    end subroutine test_extract_tabs

    subroutine test_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, grid)
        call delete_rectangle(2_int64, 19_int64, st)
        call check(error, buffer_string(), "aef"//newline//"gkl"//newline//"mqr")
        call close_buffer(h, st)
    end subroutine test_delete

    !> delete-rectangle-line splits a TAB that the start column is in
    !> (move-to-column with `coerce'); indent-tabs-mode is bound to nil.
    subroutine test_delete_tab(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, "ab cdefgh"//newline//"a"//tab//"b"//newline//"abcde")
        call delete_rectangle(3_int64, 20_int64, st)
        call check(error, buffer_string(), "abefgh"//newline//"a    b"//newline//"ab")
        if (allocated(error)) return
        call check(error, indent_tabs_mode%value, "indent-tabs-mode restored")
        call close_buffer(h, st)
    end subroutine test_delete_tab

    !> A character two columns wide across the start column is not
    !> deleted (>= in delete-rectangle-line); one across the end column is.
    subroutine test_delete_wide(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=*), parameter :: a = char(227)//char(129)//char(130), i = char(227)//char(129)//char(132), &
                                       u = char(227)//char(129)//char(134)

        ! Columns 1 to 3: "あいう" loses い (columns 2-3).
        call fresh(h, "abcdef"//newline//a//i//u//newline//"abcdef")
        call delete_rectangle(2_int64, 15_int64, st)
        call check(error, buffer_string(), "adef"//newline//a//u//newline//"adef")
        call close_buffer(h, st)
    end subroutine test_delete_wide

    subroutine test_delete_fill(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        ! Without FILL a short line is left alone; with it, it is padded.
        call fresh(h, "abcdef"//newline//"g"//newline//"mnopqr")
        call delete_rectangle(3_int64, 15_int64, st)
        call check(error, buffer_string(), "abf"//newline//"g"//newline//"mnr")
        if (allocated(error)) return
        call fresh(h, "abcdef"//newline//"g"//newline//"mnopqr")
        call delete_rectangle(3_int64, 15_int64, st, fill=.true.)
        call check(error, buffer_string(), "abf"//newline//"g "//newline//"mnr")
        call close_buffer(h, st)
    end subroutine test_delete_fill

    subroutine test_delete_extract(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(string_type), allocatable :: rectangle(:)

        call fresh(h, "abcdef"//newline//"g"//newline//"mnopqr")
        call delete_extract_rectangle(2_int64, 14_int64, rectangle, st)
        call check(error, joined(rectangle), "bcd|   |nop")
        if (allocated(error)) return
        call check(error, buffer_string(), "aef"//newline//"g"//newline//"mqr")
        if (allocated(error)) return
        ! A line ending inside the rectangle is padded (move-to-column t)
        ! and the padding is extracted with it.
        call fresh(h, "abcdef"//newline//"ghi"//newline//"mnopqr")
        call delete_extract_rectangle(2_int64, 16_int64, rectangle, st)
        call check(error, joined(rectangle), "bcd|hi |nop")
        if (allocated(error)) return
        call check(error, buffer_string(), "aef"//newline//"g"//newline//"mqr")
        call close_buffer(h, st)
    end subroutine test_delete_extract

    subroutine test_kill_paste(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, grid)
        call cut_rectangle(2_int64, 19_int64, st)
        call check(error, joined(last_cut_rectangle), "bcd|hij|nop")
        if (allocated(error)) return
        call check(error, buffer_string(), "aef"//newline//"gkl"//newline//"mqr")
        if (allocated(error)) return
        ! Paste at the start: mark at the upper left corner, point at the
        ! lower right.
        call goto_character(1_int64)
        call paste_rectangle(st)
        call check(error, buffer_string(), "bcdaef"//newline//"hijgkl"//newline//"nopmqr")
        if (allocated(error)) return
        call check(error, mark_position() == 1 .and. point() == 18, "mark and point")
        call close_buffer(h, st)
    end subroutine test_kill_paste

    subroutine test_kill_read_only(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, grid)
        if (allocated(last_cut_rectangle)) deallocate (last_cut_rectangle)
        call set_buffer_read_only(h, .true., st)
        call cut_rectangle(2_int64, 19_int64, st)
        call check(error, condition_matches(st%code, BUFFER_READ_ONLY_CODE), format_error(st))
        if (allocated(error)) return
        call check(error, joined(last_cut_rectangle), "bcd|hij|nop")
        if (allocated(error)) return
        call check(error, buffer_string(), grid)
        if (allocated(error)) return
        cut_read_only_ok = .true.
        deallocate (last_cut_rectangle)
        call cut_rectangle(2_int64, 19_int64, st)
        cut_read_only_ok = .false.
        call check(error, st%ok() .and. joined(last_cut_rectangle) == "bcd|hij|nop", format_error(st))
        if (allocated(error)) return
        call check(error, last_message, "Read only text copied to "//char(226)//char(128)//char(152)// &
                   "last_cut_rectangle"//char(226)//char(128)//char(153))
        call set_buffer_read_only(h, .false., st)
        call close_buffer(h, st)
    end subroutine test_kill_read_only

    subroutine test_insert_extends(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(string_type), allocatable :: rectangle(:)

        call append_string(rectangle, "12", "34", "56")
        ! The second line is too short and the buffer too: spaces and a
        ! newline are added.
        call fresh(h, "abcdef"//newline//"g")
        call goto_character(4_int64)
        call insert_rectangle(rectangle, st)
        call check(error, buffer_string(), "abc12def"//newline//"g  34"//newline//"   56")
        call close_buffer(h, st)
    end subroutine test_insert_extends

    subroutine test_open(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, grid)
        indent_tabs_mode%value = .false.
        call goto_character(10_int64)
        call open_rectangle(2_int64, 19_int64, st)
        call check(error, buffer_string(), "a   bcdef"//newline//"g   hijkl"//newline//"m   nopqr")
        if (allocated(error)) return
        ! Point goes to START.
        call check(error, point(), 2_int64)
        if (allocated(error)) return
        ! Without FILL, nothing is added to a line ending at the start column.
        call fresh(h, "abcdef"//newline//"g"//newline//"mnopqr")
        indent_tabs_mode%value = .false.
        call open_rectangle(2_int64, 14_int64, st)
        call check(error, buffer_string(), "a   bcdef"//newline//"g"//newline//"m   nopqr")
        if (allocated(error)) return
        call fresh(h, "abcdef"//newline//"g"//newline//"mnopqr")
        indent_tabs_mode%value = .false.
        call open_rectangle(2_int64, 14_int64, st, fill=.true.)
        call check(error, buffer_string(), "a   bcdef"//newline//"g   "//newline//"m   nopqr")
        call close_buffer(h, st)
    end subroutine test_open

    subroutine test_clear(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, grid)
        indent_tabs_mode%value = .false.
        call clear_rectangle(2_int64, 19_int64, st)
        call check(error, buffer_string(), "a   ef"//newline//"g   kl"//newline//"m   qr")
        if (allocated(error)) return
        ! A line ending inside the rectangle is cut at the start column
        ! unless FILL.
        call fresh(h, "abcdef"//newline//"ghi"//newline//"mnopqr")
        indent_tabs_mode%value = .false.
        call clear_rectangle(2_int64, 16_int64, st)
        call check(error, buffer_string(), "a   ef"//newline//"g"//newline//"m   qr")
        if (allocated(error)) return
        call fresh(h, "abcdef"//newline//"ghi"//newline//"mnopqr")
        indent_tabs_mode%value = .false.
        call clear_rectangle(2_int64, 16_int64, st, fill=.true.)
        call check(error, buffer_string(), "a   ef"//newline//"g   "//newline//"m   qr")
        call close_buffer(h, st)
    end subroutine test_clear

    subroutine test_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, grid)
        call string_rectangle(2_int64, 19_int64, "XY", st)
        call check(error, buffer_string(), "aXYef"//newline//"gXYkl"//newline//"mXYqr")
        if (allocated(error)) return
        ! Point ends after the last string.
        call check(error, point(), 16_int64)
        if (allocated(error)) return
        ! Lines too short are extended to the start column.
        call fresh(h, "abcdef"//newline//newline//"mnopqr")
        indent_tabs_mode%value = .false.
        call string_rectangle(3_int64, 12_int64, "-", st)
        call check(error, buffer_string(), "ab-def"//newline//"  -"//newline//"mn-pqr")
        call close_buffer(h, st)
    end subroutine test_string

    subroutine test_string_insert(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, grid)
        call string_insert_rectangle(2_int64, 19_int64, "XY", st)
        call check(error, buffer_string(), "aXYbcdef"//newline//"gXYhijkl"//newline//"mXYnopqr")
        call close_buffer(h, st)
    end subroutine test_string_insert

    subroutine test_delete_whitespace(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, "a   b"//newline//"c"//tab//"d"//newline//"e"//newline//"f  g")
        call delete_whitespace_rectangle(2_int64, 15_int64, st)
        call check(error, buffer_string(), "ab"//newline//"cd"//newline//"e"//newline//"fg")
        call close_buffer(h, st)
    end subroutine test_delete_whitespace

    subroutine test_number_lines(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, "a"//newline//"b"//newline//"c")
        call check(error, count_lines(1_int64, 5_int64), 2_int64)
        if (allocated(error)) return
        call rectangle_number_lines(1_int64, 5_int64, 1_int64, st)
        call check(error, buffer_string(), "1 a"//newline//"2 b"//newline//"3 c")
        if (allocated(error)) return
        ! The width of the default format comes from the last number.
        call fresh(h, "a"//newline//"b"//newline//"c")
        call rectangle_number_lines(1_int64, 5_int64, 9_int64, st)
        call check(error, buffer_string(), " 9 a"//newline//"10 b"//newline//"11 c")
        if (allocated(error)) return
        call fresh(h, "a"//newline//"b")
        call rectangle_number_lines(1_int64, 3_int64, 7_int64, st, '(i3.3, ": ")')
        call check(error, buffer_string(), "007: a"//newline//"008: b")
        if (allocated(error)) return
        call fresh(h, "a"//newline//"b")
        call rectangle_number_lines(1_int64, 3_int64, 254_int64, st, '(z0, "|", 1x)')
        call check(error, buffer_string(), "FE| a"//newline//"FF| b")
        if (allocated(error)) return
        call fresh(h, "a")
        call rectangle_number_lines(1_int64, 1_int64, 1_int64, st, "%d")
        call check(error, format_error(st), "Invalid format string: %d")
        if (allocated(error)) return
        call check(error, buffer_string(), "a")
        call close_buffer(h, st)
    end subroutine test_number_lines

    subroutine test_geometry(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: w, ht, column, line
        integer(int64), allocatable :: starts(:), ends(:)

        call fresh(h, grid)
        call rectangle_dimensions(2_int64, 19_int64, w, ht)
        call check(error, w == 3 .and. ht == 3, "dimensions")
        if (allocated(error)) return
        call rectangle_position_as_coordinates(19_int64, column, line)
        call check(error, column == 4 .and. line == 3 .and. line_number_at_position() == 1, "coordinates")
        if (allocated(error)) return
        call check(error, is_rectangle_intersect([0_int64, 0_int64], [2_int64, 2_int64], [1_int64, 1_int64], &
                                                 [2_int64, 2_int64]), "intersect")
        if (allocated(error)) return
        call check(error,.not. is_rectangle_intersect([0_int64, 0_int64], [1_int64, 2_int64], [1_int64, 0_int64], &
                                                      [2_int64, 2_int64]), "no intersect")
        if (allocated(error)) return
        call extract_rectangle_bounds(2_int64, 19_int64, starts, ends, st)
        call check(error, all(starts == [2_int64, 9_int64, 16_int64]) .and. all(ends == [5_int64, 12_int64, 19_int64]), &
                   "bounds")
        if (allocated(error)) return
        call check(error, len(spaces_string(12_int64)) == 12 .and. spaces_string(0_int64) == "", "spaces-string")
        call close_buffer(h, st)
    end subroutine test_geometry

    !> The C-x r keys, the prefix argument as FILL, and C-x r t's prompt
    !> with its history default.
    subroutine test_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=*), parameter :: cxr = achar(24)//"r"

        call fresh(h, grid)
        call discard_input()
        prefix_argument = prefix_argument_type()
        call initialize_bindings()
        call set(value_type(name="string_rectangle_history"), value_type(), st)
        ! C-SPC at b, to q: C-x r k, then C-x r y at the start.
        call keyboard_store_bytes(achar(6)//achar(0)//achar(14)//achar(14)//achar(6)//achar(6)//achar(6)//cxr//"k")
        call command_loop(st)
        call check(error, buffer_string(), "aef"//newline//"gkl"//newline//"mqr")
        if (allocated(error)) return
        call keyboard_store_bytes(achar(27)//"<"//cxr//"y")
        call command_loop(st)
        call check(error, buffer_string(), "bcdaef"//newline//"hijgkl"//newline//"nopmqr")
        if (allocated(error)) return
        ! C-x r M-w copies; C-x r d deletes; C-x r o opens.
        call goto_character(1_int64)
        call set_mark(1_int64)
        call goto_character(17_int64)
        call keyboard_store_bytes(cxr//achar(27)//"w")
        call command_loop(st)
        call check(error, joined(last_cut_rectangle), "bc|hi|no")
        if (allocated(error)) return
        call set_mark(1_int64)
        call goto_character(17_int64)
        call keyboard_store_bytes(cxr//"d")
        call command_loop(st)
        call check(error, buffer_string(), "daef"//newline//"jgkl"//newline//"pmqr")
        if (allocated(error)) return
        ! C-x r t with an empty history: no default in the prompt.
        call set_mark(1_int64)
        call goto_character(11_int64)
        call keyboard_store_bytes(cxr//"t"//"<>"//achar(13))
        call command_loop(st)
        call check(error, buffer_string(), "<>daef"//newline//"<>jgkl"//newline//"<>pmqr")
        if (allocated(error)) return
        ! The second time the default is the last string.
        call set_mark(3_int64)
        call goto_character(18_int64)
        call keyboard_store_bytes(cxr//"t"//achar(13))
        call command_loop(st)
        call check(error, buffer_string(), "<><>aef"//newline//"<><>gkl"//newline//"<><>mqr")
        if (allocated(error)) return
        ! C-x r N numbers the lines.
        call set_mark(1_int64)
        call goto_character(17_int64)
        call keyboard_store_bytes(cxr//"N")
        call command_loop(st)
        call check(error, buffer_string(), "1 <><>aef"//newline//"2 <><>gkl"//newline//"3 <><>mqr")
        if (allocated(error)) return
        ! C-x r c with C-u fills.
        call erase_buffer(st)
        call insert("abcdef"//newline//"g"//newline//"mnopqr", st)
        indent_tabs_mode%value = .false.
        call set_mark(2_int64)
        call goto_character(14_int64)
        call keyboard_store_bytes(achar(21)//cxr//"c")
        call command_loop(st)
        call check(error, buffer_string(), "a   ef"//newline//"g   "//newline//"m   qr")
        call close_buffer(h, st)
    end subroutine test_keys

    !> C-x SPC, the remapped motion keys, C-w cutting the rectangle
    !> (Rectangle-Mark mode ends with the region) and C-y pasting it back
    !> as a rectangle (its paste handler).
    subroutine test_mark_mode_kill_paste(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text

        call fresh(h, grid)
        call discard_input()
        prefix_argument = prefix_argument_type()
        call initialize_bindings()
        call goto_character(2_int64)
        call keyboard_store_bytes(achar(24)//" ")
        call command_loop(st)
        call check(error, minor_modes%rectangle_mark_mode%enabled(), "mode on")
        if (allocated(error)) return
        call check(error, mark_position() == 2 .and. is_region_active(), "mark set at b")
        if (allocated(error)) return
        call check(error, last_message == "Mark set (rectangle mode)", last_message)
        if (allocated(error)) return
        call keyboard_store_bytes(achar(14)//achar(14)//achar(6)//achar(6))
        call command_loop(st)
        call check(error, point() == 18, "at p")
        if (allocated(error)) return
        call keyboard_store_bytes(achar(23))
        call command_loop(st)
        call check(error, buffer_string(), "adef"//newline//"gjkl"//newline//"mpqr")
        if (allocated(error)) return
        call check(error,.not. minor_modes%rectangle_mark_mode%enabled(), "mode off with the region")
        if (allocated(error)) return
        call current_clipboard_entry(0_int64, text, st)
        call check(error, text, "bc"//newline//"hi"//newline//"no")
        if (allocated(error)) return
        call keyboard_store_bytes(achar(27)//"<"//achar(25))
        call command_loop(st)
        call check(error, buffer_string(), "bcadef"//newline//"higjkl"//newline//"nompqr")
        call close_buffer(h, st)
    end subroutine test_mark_mode_kill_paste

    !> A corner past the end of a short line: point's crutch keeps its
    !> column, and M-w copies the rectangle padded with spaces.
    subroutine test_mark_mode_past_end_of_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text

        call fresh(h, "abcd"//newline//"ab"//newline//"abcd")
        call discard_input()
        prefix_argument = prefix_argument_type()
        call initialize_bindings()
        call keyboard_store_bytes(achar(24)//" "//achar(6)//achar(6)//achar(6)//achar(14))
        call command_loop(st)
        call check(error, point() == 8, "at the end of ab")
        if (allocated(error)) return
        call keyboard_store_bytes(achar(14))
        call command_loop(st)
        call check(error, point() == 12, "column 3 again")
        if (allocated(error)) return
        call keyboard_store_bytes(achar(27)//"w")
        call command_loop(st)
        call current_clipboard_entry(0_int64, text, st)
        call check(error, text, "abc"//newline//"ab "//newline//"abc")
        if (allocated(error)) return
        call check(error, buffer_string(), "abcd"//newline//"ab"//newline//"abcd")
        call close_buffer(h, st)
    end subroutine test_mark_mode_past_end_of_line

    !> In Rectangle-Mark mode `region_beginning' and `region_end' are the
    !> top left and bottom right corners, the region has a piece on each
    !> line, and C-x C-x cycles through the corners.
    subroutine test_mark_mode_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: b, e
        integer(int64), allocatable :: starts(:), ends(:)

        call fresh(h, grid)
        call discard_input()
        prefix_argument = prefix_argument_type()
        call initialize_bindings()
        call goto_character(4_int64)
        call keyboard_store_bytes(achar(24)//" "//achar(14)//achar(14)//achar(2)//achar(2))
        call command_loop(st)
        call check(error, point() == 16 .and. mark_position() == 4, "d to n")
        if (allocated(error)) return
        call region_beginning(b, st)
        call region_end(e, st)
        call check(error, b == 2 .and. e == 18, "b and p")
        if (allocated(error)) return
        call interactive_region(b, e, st)
        call check(error, b == 4 .and. e == 16, "point and mark for `r'")
        if (allocated(error)) return
        call check(error, is_region_noncontiguous(), "noncontiguous")
        if (allocated(error)) return
        call region_bounds(starts, ends, st)
        call check(error, size(starts) == 3, "three pieces")
        if (allocated(error)) return
        call check(error, all(starts == [2_int64, 9_int64, 16_int64]) .and. all(ends == [4_int64, 11_int64, 18_int64]), &
                   "bc, hi and no")
        if (allocated(error)) return
        call keyboard_store_bytes(achar(24)//achar(24))
        call command_loop(st)
        call check(error, point() == 4 .and. mark_position() == 16, "exchanged")
        if (allocated(error)) return
        call keyboard_store_bytes(achar(24)//achar(24))
        call command_loop(st)
        call check(error, point() == 18 .and. mark_position() == 2, "the other corners")
        if (allocated(error)) return
        call check(error, minor_modes%rectangle_mark_mode%enabled(), "still on")
        call deactivate_mark()
        call check(error,.not. minor_modes%rectangle_mark_mode%enabled(), "off with the region")
        call close_buffer(h, st)
    end subroutine test_mark_mode_region
end module test_rectangle_mod
