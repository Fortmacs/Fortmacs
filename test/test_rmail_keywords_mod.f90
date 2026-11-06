! test_rmail_keywords_mod.f90 -- tests of rmail_keywords_mod.
! Own tests; upstream has no rmailkwd-tests.el.
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

!> Labels: adding and removing a label of the current message, setting
!> an attribute by its name, and moving to the next message with a label.
module test_rmail_keywords_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use rmail_mod, only: rmail_states, get_rmail_header, get_rmail_labels, is_rmail_message_deleted, show_first_rmail_message
    use rmail_keywords_mod, only: add_rmail_label, remove_rmail_label, set_rmail_label, next_rmail_labeled_message, &
                                  rmail_labels
    use test_rmail_support_mod, only: set_up_rmail, leave_rmail, visit_rmail, rmail_state, SAMPLE_MBOX
    implicit none(type, external)
    private

    public :: collect_rmail_keywords_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_rmail_keywords_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("rmail-add-remove-label", test_add_remove), &
                    new_unittest("rmail-label-attribute", test_attribute_label), &
                    new_unittest("rmail-next-labeled-message", test_next_labeled) &
                    ]
    end subroutine collect_rmail_keywords_mod

    !> a adds a label to X-RMAIL-KEYWORDS; k removes it.
    subroutine test_add_remove(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file, value, labels
        type(status_type) :: st

        call set_up_rmail(file)
        call visit_rmail(file, SAMPLE_MBOX, st)
        call add_rmail_label("work", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call add_rmail_label("urgent", st)
        call get_rmail_header("X-RMAIL-KEYWORDS", value, st)
        call check(error, value == "work, urgent", value)
        if (allocated(error)) return
        call get_rmail_labels(labels, st)
        call check(error, labels == " work, urgent", labels)
        if (allocated(error)) return
        call remove_rmail_label("work", st)
        call get_rmail_header("X-RMAIL-KEYWORDS", value, st)
        call check(error, value == "urgent", value)
        if (allocated(error)) return
        call remove_rmail_label("urgent", st)
        call get_rmail_header("X-RMAIL-KEYWORDS", value, st)
        call check(error,.not. allocated(value), "the header is removed")
        if (allocated(error)) return
        call set_rmail_label("a,b", .true., st)
        call check(error, format_error(st) == "More than one label specified", format_error(st))
        call leave_rmail(file)
    end subroutine test_add_remove

    !> A label that names an attribute sets the attribute.
    subroutine test_attribute_label(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file
        type(status_type) :: st

        call set_up_rmail(file)
        call visit_rmail(file, SAMPLE_MBOX, st)
        call add_rmail_label("deleted", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, is_rmail_message_deleted(1_int64))
        if (allocated(error)) return
        call check(error, rmail_labels(1)%text == "answered", "the attribute names are labels")
        call leave_rmail(file)
    end subroutine test_attribute_label

    !> C-M-n moves to the next message with one of the labels.
    subroutine test_next_labeled(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file
        type(status_type) :: st
        integer :: k

        call set_up_rmail(file)
        call visit_rmail(file, SAMPLE_MBOX, st)
        k = rmail_state(file)
        call set_rmail_label("todo", .true., st, 2_int64)
        call next_rmail_labeled_message(1_int64, "todo, later", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, rmail_states(k)%current_message == 2_int64)
        if (allocated(error)) return
        call next_rmail_labeled_message(1_int64, "", st)
        call check(error, format_error(st) == "No following message with labels todo, later", format_error(st))
        call leave_rmail(file)
    end subroutine test_next_labeled
end module test_rmail_keywords_mod
