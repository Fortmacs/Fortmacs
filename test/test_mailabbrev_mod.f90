! test_mailabbrev_mod.f90 -- tests for mailabbrev_mod.
! Own tests; upstream has no mailabbrev-tests.el.
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

!> Mail aliases as abbrevs: reading a .mailrc file (comments,
!> continuation lines, quoted addresses, "source"), resolving aliases
!> that refer to others, the alias loop error, the quoting of full names
!> in `define_mail_abbrev', and expansion in the address fields of a
!> Mail mode buffer only.
module test_mailabbrev_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: get_buffer, close_buffer, is_buffer_live
    use insdel_mod, only: insert
    use editfns_mod, only: buffer_string, goto_char, point_max
    use bindings_mod, only: init_bindings
    use files_mod, only: create_temp_file, delete_file
    use fileio_mod, only: file_exists
    use abbrev_mod, only: expand_abbrev
    use sendmail_mod, only: mail, mail_personal_alias_file, set_up_mail_with_from, mail_signature_variable, &
                            MAIL_SIGNATURE_NO, MAIL_NOERASE_NO
    use mailabbrev_mod, only: build_mail_abbrevs, define_mail_abbrev, resolve_all_mail_aliases, set_up_mail_abbrevs, &
                              mail_abbrevs, mail_abbrevs_built
    implicit none(type, external)
    private

    public :: collect_mailabbrev_mod

    character(len=*), parameter :: LF = achar(10)

contains

    !> Collect all exported unit tests.
    subroutine collect_mailabbrev_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("build-mail-abbrevs", test_build), &
                    new_unittest("define-mail-abbrev-quoting", test_define), &
                    new_unittest("mail-alias-loop", test_loop), &
                    new_unittest("mail-abbrev-expansion-in-header", test_expansion) &
                    ]
    end subroutine collect_mailabbrev_mod

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> No aliases, and an alias file that does not exist (so that the
    !> user's ~/.mailrc is never read).
    subroutine set_up(file)
        character(len=:), allocatable, intent(out) :: file
        type(status_type) :: st

        call init_bindings()
        call create_temp_file("mailabbrev-test", file, st)
        call delete_file(file, st)
        mail_personal_alias_file = file
        mail_abbrevs_built = .false.
        call mail_abbrevs%clear()
        set_up_mail_with_from = .false.
        mail_signature_variable = MAIL_SIGNATURE_NO
    end subroutine set_up

    subroutine leave(file)
        character(len=*), intent(in) :: file
        type(status_type) :: st
        type(handle_type) :: h

        if (file_exists(file)) call delete_file(file, st)
        mail_abbrevs_built = .false.
        call mail_abbrevs%clear()
        h = get_buffer("*mail*")
        if (is_buffer_live(h)) call close_buffer(h, st)
    end subroutine leave

    !> Write TEXT to FILE.
    subroutine write_file(file, text)
        character(len=*), intent(in) :: file, text
        integer :: unit

        open (newunit=unit, file=file, access="stream", form="unformatted", status="replace", action="write")
        write (unit) text
        close (unit)
    end subroutine write_file

    !> The expansion of ALIAS ("" if none).
    function expansion_of(alias) result(text)
        character(len=*), intent(in) :: alias
        character(len=:), allocatable :: text
        integer :: k

        text = ""

        do k = 1, mail_abbrevs%n
            if (mail_abbrevs%abbrevs(k)%name /= alias) cycle
            if (allocated(mail_abbrevs%abbrevs(k)%expansion)) text = mail_abbrevs%abbrevs(k)%expansion
        end do
    end function expansion_of

    subroutine check_string(error, got, wanted)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: got, wanted

        call check(error, got == wanted, "got """//got//""", wanted """//wanted//"""")
    end subroutine check_string

    ! ------------------------------------------------------------------
    ! Tests
    ! ------------------------------------------------------------------

    subroutine test_build(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file, sourced
        type(status_type) :: st

        call set_up(file)
        call create_temp_file("mailabbrev-source", sourced, st)
        call write_file(sourced, "alias src s@x"//LF)
        call write_file(file, "# my aliases"//LF// &
                        "alias joe joe@x"//LF// &
                        "alias Team joe ann@y"//LF// &
                        'alias quoted "Ann Lee <ann@y>" bob@z \'//LF// &
                        "  carl@w"//LF// &
                        "group g joe"//LF// &
                        "source "//sourced)
        call build_mail_abbrevs(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, mail_abbrevs_built, "not built")
        if (.not. allocated(error)) call check_string(error, expansion_of("joe"), "joe@x")
        if (.not. allocated(error)) call check_string(error, expansion_of("team"), "joe@x, ann@y")
        if (.not. allocated(error)) call check_string(error, expansion_of("quoted"), "Ann Lee <ann@y>, bob@z, carl@w")
        if (.not. allocated(error)) call check_string(error, expansion_of("g"), "joe@x")
        if (.not. allocated(error)) call check_string(error, expansion_of("src"), "s@x")
        call delete_file(sourced, st)
        call leave(file)
    end subroutine test_build

    subroutine test_define(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file
        type(status_type) :: st

        call set_up(file)
        call define_mail_abbrev("Pals", "Foo.Bar <f@b>, c@d", st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check_string(error, expansion_of("pals"), '"Foo.Bar" <f@b>, c@d')
        if (allocated(error)) return
        call define_mail_abbrev("smith", ' "Smith, J" <j@s>, k@l ,', st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check_string(error, expansion_of("smith"), '"Smith, J" <j@s>, k@l')
        call leave(file)
    end subroutine test_define

    subroutine test_loop(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file
        type(status_type) :: st

        call set_up(file)
        call define_mail_abbrev("a", "b", st)
        call define_mail_abbrev("b", "a", st)
        call resolve_all_mail_aliases(st)
        call check(error,.not. st%ok(), "no error")
        if (.not. allocated(error)) call check(error, index(format_error(st), "Mail alias loop detected: ") > 0, &
                                               format_error(st))
        call leave(file)
    end subroutine test_loop

    subroutine test_expansion(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file
        type(status_type) :: st

        call set_up(file)
        call write_file(file, "alias joe joe@x"//LF)
        call mail(st, MAIL_NOERASE_NO)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set_up_mail_abbrevs(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        ! In the To field (point is after "To: ").
        call insert("joe", st)
        call expand_abbrev(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, index(buffer_string(), "To: joe@x"//LF) == 1, buffer_string())
        if (allocated(error)) return
        ! Not in the text.
        call goto_char(point_max())
        call insert("joe", st)
        call expand_abbrev(st)
        call check_string(error, buffer_string(), "To: joe@x"//LF//"Subject: "//LF//"--text follows this line--"//LF//"joe")
        call leave(file)
    end subroutine test_expansion
end module test_mailabbrev_mod
