! test_minibuf_fm.f90 -- tests for the minibuffer (minibuf_fm,
! minibuffer_fm, minibuffer history in simple_fm, yes-or-no-p, fields).
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

!> Own tests: minibuf-tests.el and minibuffer-tests.el need Lisp.
module test_minibuf_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error, FM_QUIT, FM_USER_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_fixnum, nil, list_length, list_elt, xstring
    use lread_fm, only: sym
    use data_fm, only: set, symbol_value
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, current_buffer, kill_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: buffer_string
    use keymap_fm, only: key_parse, define_key
    use callint_fm, only: defcommand
    use keyboard_fm, only: unread_command_events, discard_input, last_message, command_loop
    use frame_fm, only: selected_frame
    use bindings_fm, only: init_bindings
    use minibuf_fm, only: minibuf_level, read_from_minibuffer, read_string, minibuffer_local_map
    use minibuffer_fm, only: read_file_name
    use fns_fm, only: yes_or_no_p
    use subr_fm, only: add_to_history, history_length
    use fileio_fm, only: init_fileio, emacs_wd, file_name_as_directory, write_region, &
                         make_directory_internal, delete_file_internal, file_exists_p
    use files_fm, only: set_visited_file_name, write_file
    implicit none(type, external)
    private

    public :: collect_minibuf_fm

    !> What the nested-minibuffer command saw.
    type(status_type) :: nested_status

contains

    subroutine collect_minibuf_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("abort-with-C-g", test_abort), &
                    new_unittest("read-from-minibuffer", test_read), &
                    new_unittest("prompt-is-protected", test_prompt), &
                    new_unittest("history", test_history), &
                    new_unittest("future-history-default", test_default), &
                    new_unittest("add-to-history", test_add_to_history), &
                    new_unittest("no-recursive-minibuffer", test_no_recursion), &
                    new_unittest("yes-or-no-p", test_yes_or_no), &
                    new_unittest("read-file-name", test_read_file_name), &
                    new_unittest("find-file-command", test_find_file_command), &
                    new_unittest("write-file", test_write_file) &
                    ]
    end subroutine collect_minibuf_fm

    !> Queue the keys described by `keys` (kbd syntax).
    subroutine type_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st

        call key_parse(keys, evs, st)
        call unread_command_events(evs)
    end subroutine type_keys

    !> Queue the characters of `text` as input.
    subroutine type_text(text)
        character(len=*), intent(in) :: text
        type(value_type), allocatable :: evs(:)
        integer :: i

        allocate (evs(len(text)))
        do i = 1, len(text)
            evs(i) = make_fixnum(int(iachar(text(i:i)), int64))
        end do
        call unread_command_events(evs)
    end subroutine type_text

    subroutine fresh_scratch()
        type(handle_type) :: buf
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        call get_buffer_create("*scratch*", buf, st)
        call set_buffer(buf, st)
        last_message = ""
    end subroutine fresh_scratch

    subroutine test_read(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st
        type(handle_type) :: cb

        call fresh_scratch()
        call type_text("hello"//achar(13))
        call read_from_minibuffer("Say: ", answer, st)
        cb = current_buffer()
        call check(error, st%ok() .and. answer == "hello", answer)
        if (allocated(error)) return
        call check(error, buffers(cur)%name == "*scratch*" .and. minibuf_level == 0 .and. &
                   .not. selected_frame%minibuffer_selected, "back where it started")
        if (allocated(error)) return
        ! Initial input, edited.
        call type_text(achar(127)//"p"//achar(10))
        call read_string("Say: ", answer, st, initial="hello")
        call check(error, answer == "hellp", answer)
        if (allocated(error)) return
        ! read-string's default for empty input.
        call type_text(achar(13))
        call read_string("Say: ", answer, st, default="dflt")
        call check(error, answer == "dflt", answer)
    end subroutine test_read

    subroutine test_abort(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: answer

        call fresh_scratch()
        call type_keys("C-x C-f a b C-g")
        call command_loop(st)
        call check(error, minibuf_level == 0 .and. .not. selected_frame%minibuffer_selected, "left the minibuffer")
        if (allocated(error)) return
        call check(error, last_message == "Quit", last_message)
        if (allocated(error)) return
        call check(error, buffers(cur)%name == "*scratch*", buffers(cur)%name)
        if (allocated(error)) return
        call type_keys("x C-g")
        call read_from_minibuffer("Say: ", answer, st)
        call check(error, st%code == FM_QUIT .and. len(answer) == 0)
    end subroutine test_abort

    !> C-a stops after the prompt; DEL there and C-b into it don't
    !> change the prompt, and typing still lands in the input.
    subroutine test_prompt(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st

        call fresh_scratch()
        call type_keys("b C-a DEL C-b C-b a RET")
        call read_from_minibuffer("Say: ", answer, st)
        call check(error, st%ok() .and. answer == "ab", answer)
    end subroutine test_prompt

    subroutine test_history(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st

        call fresh_scratch()
        call set(sym("test-history"), nil(), st)
        call type_text("one"//achar(13))
        call read_from_minibuffer("? ", answer, st, history="test-history")
        call type_text("two"//achar(13))
        call read_from_minibuffer("? ", answer, st, history="test-history")
        call type_keys("M-p M-p RET")
        call read_from_minibuffer("? ", answer, st, history="test-history")
        call check(error, answer == "one", answer)
        if (allocated(error)) return
        ! Back to the typed input.
        call type_keys("x M-p M-n RET")
        call read_from_minibuffer("? ", answer, st, history="test-history")
        call check(error, answer == "x", answer)
        if (allocated(error)) return
        ! Past the oldest element ("Beginning of history" is reported and
        ! the input stays).
        call type_keys("<up> <up> <up> <up> RET")
        call read_from_minibuffer("? ", answer, st, history="test-history")
        call check(error, answer == "one", answer)
    end subroutine test_history

    subroutine test_default(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st

        call fresh_scratch()
        call type_keys("M-n RET")
        call read_from_minibuffer("? ", answer, st, default="future")
        call check(error, answer == "future", answer)
        if (allocated(error)) return
        call type_keys("a M-n RET")
        call read_from_minibuffer("? ", answer, st, history="empty-history")
        call check(error, answer == "a", answer)
    end subroutine test_default

    subroutine test_add_to_history(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: h
        type(status_type) :: st
        integer :: i, saved
        character(len=8) :: num

        call set(sym("h-test"), nil(), st)
        call add_to_history("h-test", "a")
        call add_to_history("h-test", "a")
        call add_to_history("h-test", "b")
        call symbol_value(sym("h-test"), h, st)
        call check(error, list_length(h) == 2 .and. xstring(list_elt(h, 1_int64)) == "b")
        if (allocated(error)) return
        saved = history_length
        history_length = 3
        do i = 1, 5
            write (num, "(i0)") i
            call add_to_history("h-test", trim(num))
        end do
        history_length = saved
        call symbol_value(sym("h-test"), h, st)
        call check(error, list_length(h) == 3 .and. xstring(list_elt(h, 1_int64)) == "5")
    end subroutine test_add_to_history

    subroutine nested_read(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: answer

        call read_from_minibuffer("Inner: ", answer, nested_status)
        status = status_type()
    end subroutine nested_read

    subroutine test_no_recursion(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(value_type), allocatable :: key(:)
        type(status_type) :: st

        call fresh_scratch()
        call defcommand("test-nested-read", nested_read)
        call key_parse("C-t", key, st)
        call define_key(minibuffer_local_map, key, sym("test-nested-read"), st)
        call type_keys("C-t RET")
        call read_from_minibuffer("Outer: ", answer, st)
        call check(error, st%ok() .and. nested_status%code == FM_USER_ERROR .and. &
                   format_error(nested_status) == "Command attempted to use minibuffer while in minibuffer", &
                   format_error(nested_status))
    end subroutine test_no_recursion

    subroutine test_yes_or_no(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        logical :: answer

        call fresh_scratch()
        call type_text("yes"//achar(13))
        call yes_or_no_p("Really?", answer, st)
        call check(error, st%ok() .and. answer)
        if (allocated(error)) return
        call type_text("NO"//achar(13))
        call yes_or_no_p("Really? ", answer, st)
        call check(error, st%ok() .and. .not. answer)
        if (allocated(error)) return
        call type_keys("C-g")
        call yes_or_no_p("Really? ", answer, st)
        call check(error, st%code == FM_QUIT)
    end subroutine test_yes_or_no

    subroutine test_read_file_name(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: name
        type(status_type) :: st

        call fresh_scratch()
        buffers(cur)%directory = "/x/y/"
        ! Unchanged input gives the default.
        call type_keys("RET")
        call read_file_name("File: ", name, st, default_filename="/x/y/z")
        call check(error, name == "/x/y/z", name)
        if (allocated(error)) return
        ! Typing an absolute name after the directory means that name.
        call type_text("/etc/hosts"//achar(13))
        call read_file_name("File: ", name, st)
        call check(error, name == "/etc/hosts", name)
        if (allocated(error)) return
        call type_text("a.txt"//achar(13))
        call read_file_name("File: ", name, st)
        call check(error, name == "/x/y/a.txt", name)
    end subroutine test_read_file_name

    subroutine test_find_file_command(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: buf
        character(len=:), allocatable :: dir

        call fresh_scratch()
        call init_fileio()
        dir = file_name_as_directory(emacs_wd)//"build/test-minibuf"
        call make_directory_internal(dir, st)
        call get_buffer_create(" f-source", buf, st)
        call set_buffer(buf, st)
        call insert("data", st)
        call write_region(dir//"/f.txt", .false., st)
        call kill_buffer(buf, st)
        call fresh_scratch()
        buffers(cur)%directory = file_name_as_directory(dir)
        call type_text(achar(24)//achar(6)//"f.txt"//achar(13))
        call command_loop(st)
        call check(error, buffers(cur)%name == "f.txt" .and. buffer_string() == "data", buffers(cur)%name)
        if (allocated(error)) return
        call kill_buffer(current_buffer(), st)
    end subroutine test_find_file_command

    subroutine test_write_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: buf
        character(len=:), allocatable :: dir
        logical :: ok

        call fresh_scratch()
        call init_fileio()
        dir = file_name_as_directory(emacs_wd)//"build/test-minibuf/"
        call make_directory_internal(dir, st)
        call delete_file_internal(dir//"w1.txt", st)
        call delete_file_internal(dir//"w2.txt", st)
        call get_buffer_create("unsaved", buf, st)
        call set_buffer(buf, st)
        call insert("text", st)
        call write_file(dir//"w1.txt", .true., st)
        call check(error, st%ok() .and. file_exists_p(dir//"w1.txt"), format_error(st))
        if (allocated(error)) return
        call check(error, buffers(cur)%name == "w1.txt" .and. buffers(cur)%filename == dir//"w1.txt", &
                   buffers(cur)%name)
        if (allocated(error)) return
        ! A directory means the same file name there; overwriting asks.
        call type_text("y")
        call write_file(dir, .true., st)
        ok = st%ok()
        call check(error, ok .and. buffers(cur)%filename == dir//"w1.txt", format_error(st))
        if (allocated(error)) return
        call set_visited_file_name(dir//"w2.txt", st)
        call check(error, buffers(cur)%name == "w2.txt" .and. .not. buffers(cur)%backed_up .and. &
                   buffers(cur)%text%save_modiff < buffers(cur)%text%modiff, "renamed and modified")
        call kill_buffer(current_buffer(), st)
    end subroutine test_write_file

end module test_minibuf_fm
