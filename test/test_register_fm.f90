! test_register_fm.f90 -- tests for the register commands (register_fm).
! Ports GNU Emacs 31.1 test/lisp/register-tests.el.
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

module test_register_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error, FM_QUIT, FM_WRONG_TYPE_ARGUMENT, FM_USER_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, erase_buffer, set_buffer_read_only, &
                         current_buffer, get_buffer, is_buffer_live
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, buffer_string, mark_position, buffer_substring
    use keyboard_fm, only: kbd_store_bytes, command_loop, discard_input, last_message, unread_command_events
    use callint_fm, only: prefix_arg
    use bindings_fm, only: init_bindings
    use simple_fm, only: deactivate_mark, set_mark
    use frame_fm, only: selected_frame, selected_window
    use window_fm, only: split_window, delete_other_windows, window_leaves, windows, get_buffer_window, is_window_live
    use rect_fm, only: rectangle_line_type
    use register_fm, only: register_table, register_contents_type, get_register, set_register, point_to_register, &
                           jump_to_register, window_configuration_to_register, copy_to_register, insert_register, &
                           append_to_register, prepend_to_register, copy_rectangle_to_register, number_to_register, &
                           increment_register, file_to_register, buffer_to_register, is_register_jumpable, &
                           is_register_insertable, describe_register_1, register_describe_oneline, &
                           register_preview_default, view_register, list_registers, register_separator, &
                           REGISTER_EMPTY, REGISTER_MARKER, REGISTER_TEXT, REGISTER_NUMBER, REGISTER_RECTANGLE, &
                           REGISTER_FILE_QUERY
    implicit none(type, external)
    private

    public :: collect_register_fm

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: cxr = achar(24)//"r"
    integer(int64), parameter :: ra = iachar("a"), rb = iachar("b"), rc = iachar("c")

contains

    !> Collect all exported unit tests.
    subroutine collect_register_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("register-test-bug27634", test_bug27634), &
                    new_unittest("register--jumpable-p", test_jumpable), &
                    new_unittest("point-and-jump", test_point_and_jump), &
                    new_unittest("text-registers", test_text), &
                    new_unittest("append-and-prepend", test_append_prepend), &
                    new_unittest("number-registers", test_numbers), &
                    new_unittest("rectangle-registers", test_rectangle), &
                    new_unittest("window-configuration-register", test_window_configuration), &
                    new_unittest("file-and-buffer-registers", test_file_and_buffer), &
                    new_unittest("describe-registers", test_describe), &
                    new_unittest("view-and-list-registers", test_view_and_list), &
                    new_unittest("register-preview", test_preview), &
                    new_unittest("register-keys", test_keys) &
                    ]
    end subroutine collect_register_fm

    !> A fresh current buffer holding TEXT, point at its start, and no
    !> registers set.
    subroutine fresh(h, text)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: text
        type(status_type) :: st
        type(rectangle_line_type) :: none(0)
        integer :: k

        call get_buffer_create("*register-test*", h, st)
        call set_buffer(h, st)
        call set_buffer_read_only(h, .false., st)
        call erase_buffer(st)
        call deactivate_mark(.true.)
        call insert(text, st)
        call goto_char(1_int64)
        call show_alone()

        ! Empty every register (an empty rectangle is nil).
        if (allocated(register_table)) then
            do k = 1, size(register_table)
                call set_register(register_table(k)%name, none)
            end do
        end if

        register_separator = -1
        call discard_input()
        prefix_arg = value_type()
        call init_bindings()
    end subroutine fresh

    !> The contents of register R.
    function value_of(r) result(v)
        integer(int64), intent(in) :: r
        type(register_contents_type) :: v

        call get_register(r, v)
    end function value_of

    !> The kind of the contents of register R.
    integer function kind_of(r)
        integer(int64), intent(in) :: r
        type(register_contents_type) :: v

        call get_register(r, v)
        kind_of = v%kind
    end function kind_of

    !> The text in register R ("" if none).
    function text_of(r) result(s)
        integer(int64), intent(in) :: r
        character(len=:), allocatable :: s
        type(register_contents_type) :: v

        call get_register(r, v)
        s = ""
        if (allocated(v%text)) s = v%text
    end function text_of

    !> The number in register R.
    integer(int64) function number_of(r)
        integer(int64), intent(in) :: r
        type(register_contents_type) :: v

        call get_register(r, v)
        number_of = v%number
    end function number_of

    !> Whether no register holds anything.
    logical function all_empty()
        integer :: k

        all_empty = .true.
        if (.not. allocated(register_table)) return

        do k = 1, size(register_table)
            if (register_table(k)%value%kind /= REGISTER_EMPTY) all_empty = .false.
        end do
    end function all_empty

    !> Test for https://debbugs.gnu.org/27634 : C-g, `escape' and C-[ at
    !> the register prompt quit, and no register is set.
    subroutine test_bug27634(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(value_type) :: escape(1)
        integer :: k

        call fresh(h, "text")

        do k = 1, 3
            select case (k)
            case (1)
                call kbd_store_bytes(achar(7))
            case (2)
                escape(1) = value_type(name="escape")
                call unread_command_events(escape)
            case default
                call kbd_store_bytes(achar(27))
            end select

            call commands_point_to_register(st)
            call check(error, st%code == FM_QUIT, "quit: "//format_error(st))
            if (allocated(error)) return
            call check(error, all_empty(), "no register set")
            if (allocated(error)) return
        end do

        call kill_buffer(h, st)
    end subroutine test_bug27634

    !> Run the command point_to_register.
    subroutine commands_point_to_register(status)
        use commands_fm, only: commands
        type(status_type), intent(out) :: status

        call commands%point_to_register%execute(status)
    end subroutine commands_point_to_register

    !> The register--jumpable-p cases that apply: a marker, a window
    !> configuration, a file, a buffer and a file-query reference can be
    !> jumped to; a string cannot.  (Keyboard macros, framesets, frame
    !> configurations, senator tags and doc-view are not ported.)
    subroutine test_jumpable(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(register_contents_type) :: v

        call fresh(h, "text")
        call point_to_register(ra, st)
        call check(error, is_register_jumpable(value_of(ra)), "marker")
        if (allocated(error)) return
        call window_configuration_to_register(rb)
        call check(error, is_register_jumpable(value_of(rb)), "window configuration")
        if (allocated(error)) return
        call file_to_register("foo", rc)
        call check(error, is_register_jumpable(value_of(rc)), "file")
        if (allocated(error)) return
        call buffer_to_register(current_buffer(), rc)
        call check(error, is_register_jumpable(value_of(rc)), "buffer")
        if (allocated(error)) return
        v%kind = REGISTER_FILE_QUERY
        v%text = "quux"
        v%number = 1
        call check(error, is_register_jumpable(v), "file-query")
        if (allocated(error)) return
        call set_register(rc, "test")
        call check(error,.not. is_register_jumpable(value_of(rc)), "string")
        if (allocated(error)) return
        call check(error, is_register_insertable(value_of(rc)) .and. is_register_insertable(value_of(ra)) .and. &
                   .not. is_register_insertable(value_of(rb)), "insertable")
        call kill_buffer(h, st)
    end subroutine test_jumpable

    subroutine test_point_and_jump(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, other
        type(status_type) :: st

        call fresh(h, "hello world")
        call goto_char(7_int64)
        call point_to_register(ra, st)
        call check(error, kind_of(ra) == REGISTER_MARKER, "a marker")
        if (allocated(error)) return
        ! The marker follows insertions.
        call goto_char(1_int64)
        call insert(">> ", st)
        call goto_char(1_int64)
        call jump_to_register(ra, st)
        call check(error, point() == 10 .and. mark_position() == 1, "jumped, mark pushed")
        if (allocated(error)) return
        ! From another buffer: switches back.
        call get_buffer_create("*register-other*", other, st)
        call set_buffer(other, st)
        call show_alone()
        call jump_to_register(ra, st)
        call check(error, st%ok() .and. buffers(cur)%name == "*register-test*" .and. point() == 10, &
                   "switched")
        if (allocated(error)) return
        ! The buffer killed: the marker points nowhere.
        call kill_buffer(h, st)
        call jump_to_register(ra, st)
        call check(error, st%code == FM_USER_ERROR .and. format_error(st) == "That register"//char(226)//char(128)// &
                   char(153)//"s buffer no longer exists", format_error(st))
        if (allocated(error)) return
        call check(error, describe_register_1(ra), "Register a contains a marker in no buffer")
        if (allocated(error)) return
        ! A register without a position.
        call set_register(rb, 5_int64)
        call jump_to_register(rb, st)
        call check(error, format_error(st), "Register doesn"//char(226)//char(128)//char(153)// &
                   "t contain a buffer position or configuration")
        call kill_buffer(other, st)
    end subroutine test_point_and_jump

    subroutine test_text(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, "hello world")
        call copy_to_register(ra, 1_int64, 6_int64, st)
        call check(error, kind_of(ra) == REGISTER_TEXT .and. text_of(ra) == "hello", "copied")
        if (allocated(error)) return
        ! Insert: point before, mark after.
        call goto_char(12_int64)
        call insert_register(ra, st)
        call check(error, buffer_string() == "hello worldhello" .and. point() == 12 .and. mark_position() == 17, &
                   "inserted")
        if (allocated(error)) return
        ! With ARG, point after and mark before.
        call goto_char(1_int64)
        call insert_register(ra, st, .true.)
        call check(error, buffer_string() == "hellohello worldhello" .and. point() == 6 .and. mark_position() == 1, &
                   "inserted with arg")
        if (allocated(error)) return
        ! Copying with DELETE_FLAG deletes.
        call copy_to_register(rb, 1_int64, 6_int64, st, delete_flag=.true.)
        call check(error, buffer_string() == "hello worldhello" .and. text_of(rb) == "hello", "deleted")
        if (allocated(error)) return
        ! A number, a position and a file are inserted as upstream does.
        call set_register(rc, 42_int64)
        call goto_char(1_int64)
        call insert_register(rc, st, .true.)
        call check(error, buffer_string(), "42hello worldhello")
        if (allocated(error)) return
        call file_to_register("foo", rc)
        call insert_register(rc, st)
        call check(error, st%code == FM_WRONG_TYPE_ARGUMENT .and. &
                   format_error(st) == "Wrong type argument: expected character(len=*), got file", &
                   format_error(st))
        if (allocated(error)) return
        call insert_register(iachar("z", int64), st)
        call check(error, format_error(st), "Register does not contain text")
        call kill_buffer(h, st)
    end subroutine test_text

    subroutine test_append_prepend(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, "one two three")
        call append_to_register(ra, 1_int64, 4_int64, st)
        call append_to_register(ra, 5_int64, 8_int64, st)
        call check(error, text_of(ra), "onetwo")
        if (allocated(error)) return
        call prepend_to_register(ra, 9_int64, 14_int64, st)
        call check(error, text_of(ra), "threeonetwo")
        if (allocated(error)) return
        ! The separator register's text goes between.
        call set_register(iachar("+", int64), ", ")
        register_separator = iachar("+", int64)
        call append_to_register(ra, 1_int64, 4_int64, st)
        call check(error, text_of(ra), "threeonetwo, one")
        if (allocated(error)) return
        ! Appending with DELETE_FLAG deletes the text.
        call append_to_register(rb, 4_int64, 8_int64, st, delete_flag=.true.)
        call check(error, text_of(rb) == " two" .and. buffer_string() == "one three", "deleted")
        if (allocated(error)) return
        call set_register(rc, 3_int64)
        call append_to_register(rc, 1_int64, 4_int64, st)
        call check(error, format_error(st), "Register does not contain text")
        register_separator = -1
        call kill_buffer(h, st)
    end subroutine test_append_prepend

    subroutine test_numbers(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, "  -12 apples")
        ! Without a number, read one at point and move over it.
        call number_to_register(ra, st)
        call check(error, number_of(ra) == -12 .and. point() == 6, "read from the buffer")
        if (allocated(error)) return
        call number_to_register(rb, st)
        call check(error, kind_of(rb) == REGISTER_NUMBER .and. number_of(rb) == 0 .and. point() == 6, &
                   "no number: 0")
        if (allocated(error)) return
        call number_to_register(ra, st, 7_int64)
        call increment_register(ra, st)
        call check(error, number_of(ra), 8_int64)
        if (allocated(error)) return
        call increment_register(ra, st, -10_int64)
        call check(error, number_of(ra), -2_int64)
        if (allocated(error)) return
        call goto_char(1_int64)
        call insert_register(ra, st, .true.)
        call check(error, buffer_string(), "-2  -12 apples")
        if (allocated(error)) return
        ! Text is appended to: the region.
        call set_mark(9_int64)
        call goto_char(15_int64)
        call increment_register(rc, st)
        call check(error, text_of(rc), "apples")
        if (allocated(error)) return
        call point_to_register(rb, st)
        call increment_register(rb, st)
        call check(error, format_error(st), "Register does not contain a number or text")
        call kill_buffer(h, st)
    end subroutine test_numbers

    subroutine test_rectangle(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(register_contents_type) :: v

        call fresh(h, "abcdef"//nl//"ghijkl"//nl//"mnopqr")
        call copy_rectangle_to_register(ra, 2_int64, 19_int64, st)
        v = value_of(ra)
        call check(error, v%kind == REGISTER_RECTANGLE .and. size(v%rectangle) == 3, "copied")
        if (allocated(error)) return
        call check(error, v%rectangle(2)%text, "hij")
        if (allocated(error)) return
        call copy_rectangle_to_register(rb, 2_int64, 19_int64, st, delete_flag=.true.)
        call check(error, buffer_string(), "aef"//nl//"gkl"//nl//"mqr")
        if (allocated(error)) return
        ! Insert: the rectangle at point, point before and mark after.
        call goto_char(1_int64)
        call insert_register(rb, st)
        call check(error, buffer_string(), "bcdaef"//nl//"hijgkl"//nl//"nopmqr")
        if (allocated(error)) return
        call check(error, point() == 1 .and. mark_position() == 18, "point and mark exchanged")
        if (allocated(error)) return
        call check(error, describe_register_1(ra), "Register a contains a rectangle starting with bcd")
        if (allocated(error)) return
        call check(error, describe_register_1(ra, .true.), "Register a contains the rectangle:"//nl//"    bcd"//nl// &
                   "    hij"//nl//"    nop"//nl)
        call kill_buffer(h, st)
    end subroutine test_rectangle

    subroutine test_window_configuration(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, other, w
        type(handle_type), allocatable :: leaves(:)
        type(status_type) :: st

        call fresh(h, "hello")
        call goto_char(3_int64)
        call get_buffer_create("*register-other*", other, st)
        call split_window(selected_window(), 0, .false., .false., w, st)
        call window_configuration_to_register(ra)
        call check(error, describe_register_1(ra), &
                   "Register a contains a window configuration: *register-test*, *register-test*.")
        if (allocated(error)) return
        ! One window showing another buffer, then back.
        call delete_other_windows(selected_window(), st)
        call set_buffer(other, st)
        call show_alone()
        call jump_to_register(ra, st)
        call window_leaves(selected_frame%root_window, leaves)
        call check(error, st%ok() .and. size(leaves) == 2, "two windows again: "//format_error(st))
        if (allocated(error)) return
        call check(error, buffers(cur)%name == "*register-test*" .and. point() == 3, &
                   "buffer and point")
        if (allocated(error)) return
        call delete_other_windows(selected_window(), st)
        call kill_buffer(other, st)
        call kill_buffer(h, st)
    end subroutine test_window_configuration

    subroutine test_file_and_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, other
        type(status_type) :: st

        call fresh(h, "hello")
        call get_buffer_create("*register-other*", other, st)
        call buffer_to_register(other, ra)
        call check(error, describe_register_1(ra), "Register a contains the buffer #<buffer *register-other*>.")
        if (allocated(error)) return
        call jump_to_register(ra, st)
        call check(error, st%ok() .and. buffers(cur)%name == "*register-other*", "switched")
        if (allocated(error)) return
        call set_buffer(h, st)
        call show_alone()
        call kill_buffer(other, st)
        call jump_to_register(ra, st)
        call check(error, format_error(st), "Attempt to display deleted buffer")
        if (allocated(error)) return
        call check(error, describe_register_1(ra), "Register a contains the buffer #<killed buffer>.")
        if (allocated(error)) return
        call file_to_register("/tmp/a""b", rb)
        call check(error, describe_register_1(rb), "Register b contains the file ""/tmp/a""""b"".")
        call kill_buffer(h, st)
    end subroutine test_file_and_buffer

    subroutine test_describe(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, "hello")
        call goto_char(3_int64)
        call point_to_register(ra, st)
        call check(error, describe_register_1(ra), "Register a contains a buffer position:"//nl// &
                   "    buffer *register-test*, position 3")
        if (allocated(error)) return
        call check(error, register_describe_oneline(ra), "buffer position: buffer *register-test*, position 3")
        if (allocated(error)) return
        call check(error, register_preview_default(ra), "a: buffer position: buffer *register-test*, position 3"//nl)
        if (allocated(error)) return
        ! Text: from the first non-whitespace, at most 21 characters, up to
        ! a newline.
        call set_register(rb, "  "//nl//"  The quick brown fox jumps over"//nl//"x")
        call check(error, describe_register_1(rb), "Register b contains text starting with"//nl// &
                   "    The quick brown fox j")
        if (allocated(error)) return
        call check(error, register_describe_oneline(rb), "text starting with The quick brown fox j")
        if (allocated(error)) return
        call set_register(rb, "ab"//nl//"cd")
        call check(error, describe_register_1(rb), "Register b contains text starting with"//nl//"    ab")
        if (allocated(error)) return
        call set_register(rb, " "//nl//" ")
        call check(error, describe_register_1(rb), "Register b contains whitespace")
        if (allocated(error)) return
        call set_register(rb, "")
        call check(error, describe_register_1(rb), "Register b contains the empty string")
        if (allocated(error)) return
        call check(error, describe_register_1(rb, .true.), "Register b contains the text:"//nl)
        if (allocated(error)) return
        call set_register(rc, -7_int64)
        call check(error, register_preview_default(rc), "c: -7"//nl)
        if (allocated(error)) return
        call set_register(int(z'7F', int64), 1_int64)
        call check(error, describe_register_1(int(z'7F', int64)), "Register DEL contains 1")
        call kill_buffer(h, st)
    end subroutine test_describe

    !> The contents of the buffer NAME.
    function contents_of(name) result(s)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: s
        type(handle_type) :: old, b
        type(status_type) :: st

        s = ""
        b = get_buffer(name)
        if (.not. is_buffer_live(b)) return
        old = current_buffer()
        call set_buffer(b, st)
        s = buffer_string()
        call set_buffer(old, st)
    end function contents_of

    subroutine test_view_and_list(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h, "hello")
        call view_register(ra, st)
        call check(error, last_message, "Register a is empty")
        if (allocated(error)) return
        call set_register(rb, "some text")
        call set_register(ra, 5_int64)
        call view_register(rb, st)
        call check(error, contents_of("*Output*"), "Register b contains the text:"//nl//"some text")
        if (allocated(error)) return
        call check(error, is_window_live(get_buffer_window(get_buffer("*Output*"))), "shown")
        if (allocated(error)) return
        ! Sorted by name; empty registers left out.
        call list_registers(st)
        call check(error, contents_of("*Output*"), "Register a contains 5"//nl// &
                   "Register b contains text starting with"//nl//"    some text"//nl)
        call delete_other_windows(selected_window(), st)
        call kill_buffer(get_buffer("*Output*"), st)
        call kill_buffer(h, st)
    end subroutine test_view_and_list

    !> C-h at the register prompt shows the preview, which goes away
    !> when the register is read.
    subroutine test_preview(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(handle_type), allocatable :: leaves(:)

        call fresh(h, "hello")
        call set_register(rb, "bee")
        call set_register(ra, 1_int64)
        ! C-x r i C-h: the preview (read here before the register).
        call kbd_store_bytes(cxr//"i"//achar(8)//"b")
        call command_loop(st)
        call check(error, buffer_string(), "beehello")
        if (allocated(error)) return
        call check(error,.not. is_buffer_live(get_buffer("*Register Preview*")), "preview killed")
        if (allocated(error)) return
        call window_leaves(selected_frame%root_window, leaves)
        call check(error, size(leaves), 1)
        if (allocated(error)) return
        call kill_buffer(h, st)
    end subroutine test_preview

    !> The C-x r register keys.
    subroutine test_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text
        type(register_contents_type) :: v

        call fresh(h, "alpha beta")
        ! C-x r SPC a at 3, M-> then C-x r j a.
        call goto_char(3_int64)
        call kbd_store_bytes(cxr//" a"//achar(27)//">"//cxr//"ja")
        call command_loop(st)
        call check(error, point(), 3_int64)
        if (allocated(error)) return
        ! C-x r s b copies the region; C-x r g b inserts it.
        call set_mark(1_int64)
        call goto_char(6_int64)
        call kbd_store_bytes(cxr//"sb"//achar(27)//">"//cxr//"gb")
        call command_loop(st)
        call check(error, buffer_string() == "alpha betaalpha" .and. point() == 16, "copy and insert")
        if (allocated(error)) return
        ! C-u 3 C-x r n c, C-x r + c, C-x r i c.
        call kbd_store_bytes(achar(21)//"3"//cxr//"nc"//cxr//"+c"//cxr//"ic")
        call command_loop(st)
        call check(error, buffer_string(), "alpha betaalpha4")
        if (allocated(error)) return
        ! C-x r r d copies a rectangle; C-x r x with C-u deletes.
        call set_mark(1_int64)
        call goto_char(3_int64)
        call kbd_store_bytes(cxr//"rd"//achar(21)//cxr//"xe")
        call command_loop(st)
        call check(error, buffer_string() == "pha betaalpha4" .and. number_of(rc) == 4, "rectangle and delete")
        if (allocated(error)) return
        v = value_of(iachar("d", int64))
        call check(error, v%rectangle(1)%text, "al")
        if (allocated(error)) return
        call buffer_substring(1_int64, 4_int64, text, st)
        call check(error, text, "pha")
        call kill_buffer(h, st)
    end subroutine test_keys
end module test_register_fm
