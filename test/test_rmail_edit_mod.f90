! test_rmail_edit_mod.f90 -- tests of rmail_edit_mod.
! Own tests; upstream has no rmailedit-tests.el.
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

!> Editing a message: the edit goes back into the message collection and
!> marks the message edited, an aborted edit changes nothing, and the
!> header fields changed while editing are applied to the message.
module test_rmail_edit_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer
    use editing_functions_mod, only: buffer_string, goto_character, point_maximum, point_minimum, widen
    use insert_delete_mod, only: insert
    use rmail_mod, only: get_rmail_header, widen_rmail, next_rmail_undeleted_message
    use rmail_edit_mod, only: edit_rmail_current_message, cease_rmail_edit, abort_rmail_edit, get_rmail_edit_headers, &
                              diff_rmail_edit_headers, update_rmail_edit_headers, rmail_header_field_type, &
                              rmail_header_diff_type
    use test_rmail_support_mod, only: set_up_rmail, leave_rmail, visit_rmail, read_file_text, SAMPLE_MBOX, LF
    implicit none(type, external)
    private

    public :: collect_rmail_edit_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_rmail_edit_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("rmail-edit-cease", test_cease), &
                    new_unittest("rmail-edit-abort", test_abort), &
                    new_unittest("rmail-edit-headers", test_headers) &
                    ]
    end subroutine collect_rmail_edit_mod

    !> e then C-c C-c: the message has the new body and the edited attribute.
    subroutine test_cease(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file, value
        type(status_type) :: st

        call set_up_rmail(file)
        call visit_rmail(file, SAMPLE_MBOX, st)
        call edit_rmail_current_message(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, buffers(current)%major_mode == "rmail_edit_mode", buffers(current)%major_mode)
        if (allocated(error)) return
        call goto_character(point_maximum())
        call insert("An added line."//LF, st)
        call cease_rmail_edit(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, buffers(current)%major_mode == "rmail_mode")
        if (allocated(error)) return
        call check(error, index(buffer_string(), "From the archives."//LF//LF//"An added line."//LF) > 0, buffer_string())
        if (allocated(error)) return
        call get_rmail_header("X-RMAIL-ATTRIBUTES", value, st, 1_int64)
        call check(error, value == "--E-----", value)
        if (allocated(error)) return
        call widen_rmail(st)
        ! The quoted From line is quoted again in the collection.
        call check(error, index(buffer_string(), ">From the archives."//LF//LF//"An added line."//LF) > 0, &
                   buffer_string())
        call leave_rmail(file)
    end subroutine test_cease

    !> e then C-c C-] on a message without "From " lines: nothing changes.
    !> (Upstream compares the text after escaping "From " lines, so a
    !> message with one is marked edited.)
    subroutine test_abort(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file, before, value
        type(status_type) :: st

        call set_up_rmail(file)
        call visit_rmail(file, SAMPLE_MBOX, st)
        call next_rmail_undeleted_message(1_int64, st)
        before = buffer_string()
        call edit_rmail_current_message(st)
        call goto_character(point_maximum())
        call insert("Not kept."//LF, st)
        call abort_rmail_edit(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, buffer_string() == before, buffer_string())
        if (allocated(error)) return
        call get_rmail_header("X-RMAIL-ATTRIBUTES", value, st, 2_int64)
        call check(error, value == "--------", value)
        call leave_rmail(file)
    end subroutine test_abort

    !> The fields changed, deleted and inserted between two headers are
    !> applied to a copy of the first.
    subroutine test_headers(error)
        type(error_type), allocatable, intent(out) :: error
        type(rmail_header_field_type), allocatable :: old(:), new(:)
        type(rmail_header_diff_type) :: diff
        type(handle_type) :: h
        type(status_type) :: st

        call get_or_make_buffer(" *rmail-edit-headers*", h, st)
        call set_buffer(h, st)
        call insert("From: a@b"//LF//"To: c@d"//LF//"Subject: old"//LF//LF//"body"//LF, st)
        call get_rmail_edit_headers(old, st)
        call check(error, size(old) == 3, "three fields")
        if (allocated(error)) return
        call check(error, old(3)%name == "Subject" .and. old(3)%text == "Subject: old"//LF, "the Subject field")
        if (allocated(error)) return
        call goto_character(point_minimum())
        call widen()
        call close_buffer(h, st)
        call get_or_make_buffer(" *rmail-edit-headers*", h, st)
        call set_buffer(h, st)
        call insert("From: a@b"//LF//"Subject: new"//LF//"Cc: e@f"//LF//LF//"body"//LF, st)
        call get_rmail_edit_headers(new, st)
        call diff_rmail_edit_headers(old, new, diff)
        call check(error, size(diff%deleted) == 1 .and. size(diff%changed_old) == 1 .and. size(diff%inserted) == 1, &
                   "one of each")
        if (allocated(error)) return
        call close_buffer(h, st)
        call get_or_make_buffer(" *rmail-edit-headers*", h, st)
        call set_buffer(h, st)
        call insert("From: a@b"//LF//"To: c@d"//LF//"Subject: old"//LF//LF//"body"//LF, st)
        call update_rmail_edit_headers(diff, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        ! A field inserted after a changed one goes before it, as upstream
        ! records the changed field's markers the other way round.
        call check(error, buffer_string() == "From: a@b"//LF//"Cc: e@f"//LF//"Subject: new"//LF//LF//"body"//LF, &
                   buffer_string())
        call close_buffer(h, st)
    end subroutine test_headers
end module test_rmail_edit_mod
