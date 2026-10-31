! test_rect_fm.f90 -- tests for the rectangle commands (rect_fm).
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

module test_rect_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use lisp_fm, only: value_type
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error, condition_matches, FM_BUFFER_READ_ONLY
    use handle_fm, only: handle_type
    use data_fm, only: set
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, erase_buffer, set_buffer_read_only
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, buffer_string, mark_position, region_beginning, region_end, interactive_region
    use keyboard_fm, only: kbd_store_bytes, command_loop, discard_input, last_message
    use callint_fm, only: prefix_arg
    use bindings_fm, only: init_bindings
    use simple_fm, only: deactivate_mark, set_mark, kill_read_only_ok, count_lines, line_number_at_pos, current_kill, &
                         is_region_active, region_bounds, is_region_noncontiguous
    use modes_fm, only: minor_modes
    use indent_fm, only: indent_tabs_mode
    use rect_fm, only: rectangle_line_type, killed_rectangle, extract_rectangle, delete_rectangle, &
                       delete_extract_rectangle, kill_rectangle, yank_rectangle, insert_rectangle, open_rectangle, &
                       clear_rectangle, string_rectangle, string_insert_rectangle, delete_whitespace_rectangle, &
                       rectangle_number_lines, spaces_string, extract_rectangle_bounds, rectangle_dimensions, &
                       rectangle_position_as_coordinates, is_rectangle_intersect
    implicit none(type, external)
    private

    public :: collect_rect_fm

    character(len=1), parameter :: nl = achar(10), tab = achar(9)
    !> "abcdef\nghijkl\nmnopqr": the corners of the rectangle of columns 1
    !> to 4 are 2 (b) and 19 (q).
    character(len=*), parameter :: grid = "abcdef"//nl//"ghijkl"//nl//"mnopqr"

contains

    !> Collect all exported unit tests.
    subroutine collect_rect_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("extract-rectangle", test_extract), &
                    new_unittest("extract-rectangle-tabs", test_extract_tabs), &
                    new_unittest("delete-rectangle", test_delete), &
                    new_unittest("delete-rectangle-splits-tab", test_delete_tab), &
                    new_unittest("delete-rectangle-wide-character", test_delete_wide), &
                    new_unittest("delete-rectangle-fill", test_delete_fill), &
                    new_unittest("delete-extract-rectangle", test_delete_extract), &
                    new_unittest("kill-and-yank-rectangle", test_kill_yank), &
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
                    new_unittest("rectangle-mark-mode-kill-yank", test_mark_mode_kill_yank), &
                    new_unittest("rectangle-mark-mode-past-eol", test_mark_mode_past_eol), &
                    new_unittest("rectangle-mark-mode-region", test_mark_mode_region) &
                    ]
    end subroutine collect_rect_fm

    !> A fresh current buffer holding TEXT, point at its start.
    subroutine fresh(h, text)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: text
        type(status_type) :: st

        call get_buffer_create("*rect-test*", h, st)
        call set_buffer(h, st)
        call set_buffer_read_only(h, .false., st)
        call erase_buffer(st)
        call deactivate_mark(.true.)
        call insert(text, st)
        call goto_char(1_int64)
        indent_tabs_mode%value = .true.
        call show_alone()
    end subroutine fresh

    !> The lines of RECT joined by "|".
    function joined(rect) result(s)
        type(rectangle_line_type), intent(in) :: rect(:)
        character(len=:), allocatable :: s
        integer :: k

        s = ""

        do k = 1, size(rect)
            if (k > 1) s = s//"|"
            s = s//rect(k)%text
        end do
    end function joined

    subroutine test_extract(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(rectangle_line_type), allocatable :: rect(:)

        call fresh(h, grid)
        call goto_char(10_int64)
        call extract_rectangle(2_int64, 19_int64, rect, st)
        call check(error, joined(rect), "bcd|hij|nop")
        if (allocated(error)) return
        ! Point is kept; the start column may be the right one.
        call check(error, point(), 10_int64)
        if (allocated(error)) return
        call extract_rectangle(5_int64, 16_int64, rect, st)
        call check(error, joined(rect), "bcd|hij|nop")
        if (allocated(error)) return
        ! The lines are those from START's to END's, so START after END
        ! gives START's line only (as upstream's apply-on-rectangle).
        call extract_rectangle(19_int64, 2_int64, rect, st)
        call check(error, joined(rect), "nop")
        if (allocated(error)) return
        ! Lines too short for the rectangle are padded with spaces.
        call fresh(h, "abcdef"//nl//"g"//nl//"mnopqr")
        call extract_rectangle(2_int64, 14_int64, rect, st)
        call check(error, joined(rect), "bcd|   |nop")
        call kill_buffer(h, st)
    end subroutine test_extract

    subroutine test_extract_tabs(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(rectangle_line_type), allocatable :: rect(:)

        ! A TAB inside the rectangle becomes the spaces it takes.
        call fresh(h, "a"//tab//"b"//nl//"0123456789")
        call extract_rectangle(1_int64, 14_int64, rect, st)
        call check(error, joined(rect), "a       b|012345678")
        if (allocated(error)) return
        ! A TAB across the start column: the rectangle line starts after
        ! it, with the columns from the start column (as upstream).
        call fresh(h, "0123456789"//nl//"a"//tab//"b"//nl//"0123456789")
        call extract_rectangle(3_int64, 21_int64, rect, st)
        call check(error, joined(rect), "234|      |234")
        call kill_buffer(h, st)
    end subroutine test_extract_tabs

    subroutine test_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, grid)
        call delete_rectangle(2_int64, 19_int64, st)
        call check(error, buffer_string(), "aef"//nl//"gkl"//nl//"mqr")
        call kill_buffer(h, st)
    end subroutine test_delete

    !> delete-rectangle-line splits a TAB that the start column is in
    !> (move-to-column with `coerce'); indent-tabs-mode is bound to nil.
    subroutine test_delete_tab(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, "ab cdefgh"//nl//"a"//tab//"b"//nl//"abcde")
        call delete_rectangle(3_int64, 20_int64, st)
        call check(error, buffer_string(), "abefgh"//nl//"a    b"//nl//"ab")
        if (allocated(error)) return
        call check(error, indent_tabs_mode%value, "indent-tabs-mode restored")
        call kill_buffer(h, st)
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
        call fresh(h, "abcdef"//nl//a//i//u//nl//"abcdef")
        call delete_rectangle(2_int64, 15_int64, st)
        call check(error, buffer_string(), "adef"//nl//a//u//nl//"adef")
        call kill_buffer(h, st)
    end subroutine test_delete_wide

    subroutine test_delete_fill(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        ! Without FILL a short line is left alone; with it, it is padded.
        call fresh(h, "abcdef"//nl//"g"//nl//"mnopqr")
        call delete_rectangle(3_int64, 15_int64, st)
        call check(error, buffer_string(), "abf"//nl//"g"//nl//"mnr")
        if (allocated(error)) return
        call fresh(h, "abcdef"//nl//"g"//nl//"mnopqr")
        call delete_rectangle(3_int64, 15_int64, st, fill=.true.)
        call check(error, buffer_string(), "abf"//nl//"g "//nl//"mnr")
        call kill_buffer(h, st)
    end subroutine test_delete_fill

    subroutine test_delete_extract(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(rectangle_line_type), allocatable :: rect(:)

        call fresh(h, "abcdef"//nl//"g"//nl//"mnopqr")
        call delete_extract_rectangle(2_int64, 14_int64, rect, st)
        call check(error, joined(rect), "bcd|   |nop")
        if (allocated(error)) return
        call check(error, buffer_string(), "aef"//nl//"g"//nl//"mqr")
        if (allocated(error)) return
        ! A line ending inside the rectangle is padded (move-to-column t)
        ! and the padding is extracted with it.
        call fresh(h, "abcdef"//nl//"ghi"//nl//"mnopqr")
        call delete_extract_rectangle(2_int64, 16_int64, rect, st)
        call check(error, joined(rect), "bcd|hi |nop")
        if (allocated(error)) return
        call check(error, buffer_string(), "aef"//nl//"g"//nl//"mqr")
        call kill_buffer(h, st)
    end subroutine test_delete_extract

    subroutine test_kill_yank(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, grid)
        call kill_rectangle(2_int64, 19_int64, st)
        call check(error, joined(killed_rectangle), "bcd|hij|nop")
        if (allocated(error)) return
        call check(error, buffer_string(), "aef"//nl//"gkl"//nl//"mqr")
        if (allocated(error)) return
        ! Yank at the start: mark at the upper left corner, point at the
        ! lower right.
        call goto_char(1_int64)
        call yank_rectangle(st)
        call check(error, buffer_string(), "bcdaef"//nl//"hijgkl"//nl//"nopmqr")
        if (allocated(error)) return
        call check(error, mark_position() == 1 .and. point() == 18, "mark and point")
        call kill_buffer(h, st)
    end subroutine test_kill_yank

    subroutine test_kill_read_only(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, grid)
        if (allocated(killed_rectangle)) deallocate (killed_rectangle)
        call set_buffer_read_only(h, .true., st)
        call kill_rectangle(2_int64, 19_int64, st)
        call check(error, condition_matches(st%code, FM_BUFFER_READ_ONLY), format_error(st))
        if (allocated(error)) return
        call check(error, joined(killed_rectangle), "bcd|hij|nop")
        if (allocated(error)) return
        call check(error, buffer_string(), grid)
        if (allocated(error)) return
        kill_read_only_ok = .true.
        deallocate (killed_rectangle)
        call kill_rectangle(2_int64, 19_int64, st)
        kill_read_only_ok = .false.
        call check(error, st%ok() .and. joined(killed_rectangle) == "bcd|hij|nop", format_error(st))
        if (allocated(error)) return
        call check(error, last_message, "Read only text copied to "//char(226)//char(128)//char(152)// &
                   "killed_rectangle"//char(226)//char(128)//char(153))
        call set_buffer_read_only(h, .false., st)
        call kill_buffer(h, st)
    end subroutine test_kill_read_only

    subroutine test_insert_extends(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(rectangle_line_type) :: rect(3)

        rect(1)%text = "12"
        rect(2)%text = "34"
        rect(3)%text = "56"
        ! The second line is too short and the buffer too: spaces and a
        ! newline are added.
        call fresh(h, "abcdef"//nl//"g")
        call goto_char(4_int64)
        call insert_rectangle(rect, st)
        call check(error, buffer_string(), "abc12def"//nl//"g  34"//nl//"   56")
        call kill_buffer(h, st)
    end subroutine test_insert_extends

    subroutine test_open(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, grid)
        indent_tabs_mode%value = .false.
        call goto_char(10_int64)
        call open_rectangle(2_int64, 19_int64, st)
        call check(error, buffer_string(), "a   bcdef"//nl//"g   hijkl"//nl//"m   nopqr")
        if (allocated(error)) return
        ! Point goes to START.
        call check(error, point(), 2_int64)
        if (allocated(error)) return
        ! Without FILL, nothing is added to a line ending at the start column.
        call fresh(h, "abcdef"//nl//"g"//nl//"mnopqr")
        indent_tabs_mode%value = .false.
        call open_rectangle(2_int64, 14_int64, st)
        call check(error, buffer_string(), "a   bcdef"//nl//"g"//nl//"m   nopqr")
        if (allocated(error)) return
        call fresh(h, "abcdef"//nl//"g"//nl//"mnopqr")
        indent_tabs_mode%value = .false.
        call open_rectangle(2_int64, 14_int64, st, fill=.true.)
        call check(error, buffer_string(), "a   bcdef"//nl//"g   "//nl//"m   nopqr")
        call kill_buffer(h, st)
    end subroutine test_open

    subroutine test_clear(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, grid)
        indent_tabs_mode%value = .false.
        call clear_rectangle(2_int64, 19_int64, st)
        call check(error, buffer_string(), "a   ef"//nl//"g   kl"//nl//"m   qr")
        if (allocated(error)) return
        ! A line ending inside the rectangle is cut at the start column
        ! unless FILL.
        call fresh(h, "abcdef"//nl//"ghi"//nl//"mnopqr")
        indent_tabs_mode%value = .false.
        call clear_rectangle(2_int64, 16_int64, st)
        call check(error, buffer_string(), "a   ef"//nl//"g"//nl//"m   qr")
        if (allocated(error)) return
        call fresh(h, "abcdef"//nl//"ghi"//nl//"mnopqr")
        indent_tabs_mode%value = .false.
        call clear_rectangle(2_int64, 16_int64, st, fill=.true.)
        call check(error, buffer_string(), "a   ef"//nl//"g   "//nl//"m   qr")
        call kill_buffer(h, st)
    end subroutine test_clear

    subroutine test_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, grid)
        call string_rectangle(2_int64, 19_int64, "XY", st)
        call check(error, buffer_string(), "aXYef"//nl//"gXYkl"//nl//"mXYqr")
        if (allocated(error)) return
        ! Point ends after the last string.
        call check(error, point(), 16_int64)
        if (allocated(error)) return
        ! Lines too short are extended to the start column.
        call fresh(h, "abcdef"//nl//nl//"mnopqr")
        indent_tabs_mode%value = .false.
        call string_rectangle(3_int64, 12_int64, "-", st)
        call check(error, buffer_string(), "ab-def"//nl//"  -"//nl//"mn-pqr")
        call kill_buffer(h, st)
    end subroutine test_string

    subroutine test_string_insert(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, grid)
        call string_insert_rectangle(2_int64, 19_int64, "XY", st)
        call check(error, buffer_string(), "aXYbcdef"//nl//"gXYhijkl"//nl//"mXYnopqr")
        call kill_buffer(h, st)
    end subroutine test_string_insert

    subroutine test_delete_whitespace(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, "a   b"//nl//"c"//tab//"d"//nl//"e"//nl//"f  g")
        call delete_whitespace_rectangle(2_int64, 15_int64, st)
        call check(error, buffer_string(), "ab"//nl//"cd"//nl//"e"//nl//"fg")
        call kill_buffer(h, st)
    end subroutine test_delete_whitespace

    subroutine test_number_lines(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, "a"//nl//"b"//nl//"c")
        call check(error, count_lines(1_int64, 5_int64), 2_int64)
        if (allocated(error)) return
        call rectangle_number_lines(1_int64, 5_int64, 1_int64, st)
        call check(error, buffer_string(), "1 a"//nl//"2 b"//nl//"3 c")
        if (allocated(error)) return
        ! The width of the default format comes from the last number.
        call fresh(h, "a"//nl//"b"//nl//"c")
        call rectangle_number_lines(1_int64, 5_int64, 9_int64, st)
        call check(error, buffer_string(), " 9 a"//nl//"10 b"//nl//"11 c")
        if (allocated(error)) return
        call fresh(h, "a"//nl//"b")
        call rectangle_number_lines(1_int64, 3_int64, 7_int64, st, "%03d: ")
        call check(error, buffer_string(), "007: a"//nl//"008: b")
        if (allocated(error)) return
        call fresh(h, "a"//nl//"b")
        call rectangle_number_lines(1_int64, 3_int64, 254_int64, st, "%-4x|%1$#o|%1$c|%%")
        call check(error, buffer_string(), "fe  |0376|"//char(195)//char(190)//"|%a"//nl// &
                   "ff  |0377|"//char(195)//char(191)//"|%b")
        if (allocated(error)) return
        call fresh(h, "a")
        call rectangle_number_lines(1_int64, 1_int64, 1_int64, st, "%d %d")
        call check(error, format_error(st), "Not enough arguments for format string")
        call kill_buffer(h, st)
    end subroutine test_number_lines

    subroutine test_geometry(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: w, ht, col, line
        integer(int64), allocatable :: starts(:), ends(:)

        call fresh(h, grid)
        call rectangle_dimensions(2_int64, 19_int64, w, ht)
        call check(error, w == 3 .and. ht == 3, "dimensions")
        if (allocated(error)) return
        call rectangle_position_as_coordinates(19_int64, col, line)
        call check(error, col == 4 .and. line == 3 .and. line_number_at_pos() == 1, "coordinates")
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
        call kill_buffer(h, st)
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
        prefix_arg = value_type()
        call init_bindings()
        call set(value_type(name="string_rectangle_history"), value_type(), st)
        ! C-SPC at b, to q: C-x r k, then C-x r y at the start.
        call kbd_store_bytes(achar(6)//achar(0)//achar(14)//achar(14)//achar(6)//achar(6)//achar(6)//cxr//"k")
        call command_loop(st)
        call check(error, buffer_string(), "aef"//nl//"gkl"//nl//"mqr")
        if (allocated(error)) return
        call kbd_store_bytes(achar(27)//"<"//cxr//"y")
        call command_loop(st)
        call check(error, buffer_string(), "bcdaef"//nl//"hijgkl"//nl//"nopmqr")
        if (allocated(error)) return
        ! C-x r M-w copies; C-x r d deletes; C-x r o opens.
        call goto_char(1_int64)
        call set_mark(1_int64)
        call goto_char(17_int64)
        call kbd_store_bytes(cxr//achar(27)//"w")
        call command_loop(st)
        call check(error, joined(killed_rectangle), "bc|hi|no")
        if (allocated(error)) return
        call set_mark(1_int64)
        call goto_char(17_int64)
        call kbd_store_bytes(cxr//"d")
        call command_loop(st)
        call check(error, buffer_string(), "daef"//nl//"jgkl"//nl//"pmqr")
        if (allocated(error)) return
        ! C-x r t with an empty history: no default in the prompt.
        call set_mark(1_int64)
        call goto_char(11_int64)
        call kbd_store_bytes(cxr//"t"//"<>"//achar(13))
        call command_loop(st)
        call check(error, buffer_string(), "<>daef"//nl//"<>jgkl"//nl//"<>pmqr")
        if (allocated(error)) return
        ! The second time the default is the last string.
        call set_mark(3_int64)
        call goto_char(18_int64)
        call kbd_store_bytes(cxr//"t"//achar(13))
        call command_loop(st)
        call check(error, buffer_string(), "<><>aef"//nl//"<><>gkl"//nl//"<><>mqr")
        if (allocated(error)) return
        ! C-x r N numbers the lines.
        call set_mark(1_int64)
        call goto_char(17_int64)
        call kbd_store_bytes(cxr//"N")
        call command_loop(st)
        call check(error, buffer_string(), "1 <><>aef"//nl//"2 <><>gkl"//nl//"3 <><>mqr")
        if (allocated(error)) return
        ! C-x r c with C-u fills.
        call erase_buffer(st)
        call insert("abcdef"//nl//"g"//nl//"mnopqr", st)
        indent_tabs_mode%value = .false.
        call set_mark(2_int64)
        call goto_char(14_int64)
        call kbd_store_bytes(achar(21)//cxr//"c")
        call command_loop(st)
        call check(error, buffer_string(), "a   ef"//nl//"g   "//nl//"m   qr")
        call kill_buffer(h, st)
    end subroutine test_keys

    !> C-x SPC, the remapped motion keys, C-w killing the rectangle
    !> (Rectangle-Mark mode ends with the region) and C-y yanking it back
    !> as a rectangle (its yank handler).
    subroutine test_mark_mode_kill_yank(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text

        call fresh(h, grid)
        call discard_input()
        prefix_arg = value_type()
        call init_bindings()
        call goto_char(2_int64)
        call kbd_store_bytes(achar(24)//" ")
        call command_loop(st)
        call check(error, minor_modes%rectangle_mark_mode%enabled(), "mode on")
        if (allocated(error)) return
        call check(error, mark_position() == 2 .and. is_region_active(), "mark set at b")
        if (allocated(error)) return
        call check(error, last_message == "Mark set (rectangle mode)", last_message)
        if (allocated(error)) return
        call kbd_store_bytes(achar(14)//achar(14)//achar(6)//achar(6))
        call command_loop(st)
        call check(error, point() == 18, "at p")
        if (allocated(error)) return
        call kbd_store_bytes(achar(23))
        call command_loop(st)
        call check(error, buffer_string(), "adef"//nl//"gjkl"//nl//"mpqr")
        if (allocated(error)) return
        call check(error,.not. minor_modes%rectangle_mark_mode%enabled(), "mode off with the region")
        if (allocated(error)) return
        call current_kill(0_int64, text, st)
        call check(error, text, "bc"//nl//"hi"//nl//"no")
        if (allocated(error)) return
        call kbd_store_bytes(achar(27)//"<"//achar(25))
        call command_loop(st)
        call check(error, buffer_string(), "bcadef"//nl//"higjkl"//nl//"nompqr")
        call kill_buffer(h, st)
    end subroutine test_mark_mode_kill_yank

    !> A corner past the end of a short line: point's crutch keeps its
    !> column, and M-w copies the rectangle padded with spaces.
    subroutine test_mark_mode_past_eol(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text

        call fresh(h, "abcd"//nl//"ab"//nl//"abcd")
        call discard_input()
        prefix_arg = value_type()
        call init_bindings()
        call kbd_store_bytes(achar(24)//" "//achar(6)//achar(6)//achar(6)//achar(14))
        call command_loop(st)
        call check(error, point() == 8, "at the end of ab")
        if (allocated(error)) return
        call kbd_store_bytes(achar(14))
        call command_loop(st)
        call check(error, point() == 12, "column 3 again")
        if (allocated(error)) return
        call kbd_store_bytes(achar(27)//"w")
        call command_loop(st)
        call current_kill(0_int64, text, st)
        call check(error, text, "abc"//nl//"ab "//nl//"abc")
        if (allocated(error)) return
        call check(error, buffer_string(), "abcd"//nl//"ab"//nl//"abcd")
        call kill_buffer(h, st)
    end subroutine test_mark_mode_past_eol

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
        prefix_arg = value_type()
        call init_bindings()
        call goto_char(4_int64)
        call kbd_store_bytes(achar(24)//" "//achar(14)//achar(14)//achar(2)//achar(2))
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
        call kbd_store_bytes(achar(24)//achar(24))
        call command_loop(st)
        call check(error, point() == 4 .and. mark_position() == 16, "exchanged")
        if (allocated(error)) return
        call kbd_store_bytes(achar(24)//achar(24))
        call command_loop(st)
        call check(error, point() == 18 .and. mark_position() == 2, "the other corners")
        if (allocated(error)) return
        call check(error, minor_modes%rectangle_mark_mode%enabled(), "still on")
        call deactivate_mark()
        call check(error,.not. minor_modes%rectangle_mark_mode%enabled(), "off with the region")
        call kill_buffer(h, st)
    end subroutine test_mark_mode_region
end module test_rect_fm
