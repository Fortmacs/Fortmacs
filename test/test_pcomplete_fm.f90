! test_pcomplete_fm.f90 -- tests for pcomplete_fm.
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

!> Own tests: upstream's pcomplete-tests.el tests `pcomplete_from_help',
!> which is not ported.  Parsing arguments, completing with a command's
!> completion function and with the default one (file names).
module test_pcomplete_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use lisp_fm, only: make_string, list
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: buffer_string
    use bindings_fm, only: init_bindings
    use files_fm, only: make_temp_file
    use fileio_fm, only: init_fileio, write_region, make_directory_internal, delete_file_internal, &
                         delete_directory_internal
    use process_fm, only: process_arg_type
    use minibuf_table_fm, only: completion_table_type, completion_list_table
    use minibuffer_fm, only: completion_at_point, completion_at_point_functions
    use pcomplete_fm, only: pcomplete_parse_buffer_arguments, pcomplete_completions_at_point, pcomplete_here, &
                            pcomplete_command_functions, pcomplete_arg, pcomplete_parse_arguments
    implicit none(type, external)
    private

    public :: collect_pcomplete_fm

contains

    subroutine collect_pcomplete_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("pcomplete-parse-buffer-arguments", test_parse_buffer_arguments), &
                    new_unittest("pcomplete-command-function", test_command_function), &
                    new_unittest("pcomplete-default-entries", test_default_entries) &
                    ]
    end subroutine collect_pcomplete_fm

    !> A new buffer NAME in DIR, current, with
    !> `pcomplete_completions_at_point' as its completion function.
    subroutine pcomplete_buffer(name, dir, h)
        character(len=*), intent(in) :: name, dir
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call get_buffer_create(name, h, st)
        call set_buffer(h, st)
        buffers(cur)%directory = dir
        call show_alone()
        call completion_at_point_functions%add(pcomplete_completions_at_point, local=.true.)
    end subroutine pcomplete_buffer

    subroutine test_parse_buffer_arguments(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(process_arg_type), allocatable :: args(:)
        integer(int64), allocatable :: begins(:)

        call pcomplete_buffer(" pcomplete-parse", "/tmp/", h)
        call insert("ls  -l"//achar(9)//"dir", st)
        call pcomplete_parse_buffer_arguments(args, begins)
        call check(error, size(args) == 3, "three arguments")

        if (.not. allocated(error)) then
            call check(error, args(1)%s//"|"//args(2)%s//"|"//args(3)%s, "ls|-l|dir")
        end if

        if (.not. allocated(error)) call check(error, begins(2) == 5 .and. begins(3) == 8, "where they begin")

        if (.not. allocated(error)) then
            call check(error, pcomplete_parse_arguments(.false.), "parsed")
            if (.not. allocated(error)) call check(error, pcomplete_arg(), "ls")
        end if

        call kill_buffer(h, st)
    end subroutine test_parse_buffer_arguments

    ! The completion function of the command `fruit': its first argument
    ! is a fruit.
    recursive subroutine complete_fruit(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (pcomplete_here(fruits)) continue
    end subroutine complete_fruit

    recursive subroutine fruits(table, found)
        type(completion_table_type), intent(out) :: table
        logical, intent(out) :: found

        table = completion_list_table(list(make_string("apple"), make_string("banana")))
        found = .true.
    end subroutine fruits

    !> A command's completion function completes its arguments; a space
    !> follows a finished completion.
    subroutine test_command_function(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call pcomplete_buffer(" pcomplete-command", "/tmp/", h)
        call pcomplete_command_functions%add("fruit", complete_fruit)
        call insert("fruit ban", st)
        call completion_at_point(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), "fruit banana ")
        call kill_buffer(h, st)
    end subroutine test_command_function

    !> Without a completion function for the command, the arguments
    !> complete as file names.
    subroutine test_default_entries(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, tmp
        type(status_type) :: st
        character(len=:), allocatable :: dir

        call init_fileio()
        call make_temp_file("pcomplete", dir, st)
        call delete_file_internal(dir, st)
        call make_directory_internal(dir, st)
        call get_buffer_create(" pcomplete-tmp", tmp, st)
        call set_buffer(tmp, st)
        call write_region(dir//"/notes.txt", .false., st)
        call kill_buffer(tmp, st)
        call pcomplete_buffer(" pcomplete-entries", dir//"/", h)
        call insert("cat no", st)
        call completion_at_point(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), "cat notes.txt ")

        if (.not. allocated(error)) then
            call erase_buffer(st)
            call insert("cat zz", st)
            call completion_at_point(st)
            call check(error, buffer_string(), "cat zz")
        end if

        call kill_buffer(h, st)
        call delete_file_internal(dir//"/notes.txt", st)
        call delete_directory_internal(dir, st)
    end subroutine test_default_entries
end module test_pcomplete_fm
