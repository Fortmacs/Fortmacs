! startup_fm.f90 -- the user options of startup.el that other modules
! read.
! Ported from GNU Emacs 31.1 lisp/startup.el.
!
! Copyright (C) 1985-1986, 1992, 1994-2026 Free Software Foundation,
! Inc.
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

!> The mail address options of startup.el: `user_mail_address' (its
!> delayed initialization: computed after the init file unless that
!> set it) and `mail_host_address'.  The rest of startup.el is in
!> emacs_fm, which uses everything; this module is for those that only
!> need these options.
module startup_fm
    use editfns_fm, only: user_login_name, system_name
    implicit none(type, external)
    private

    public :: init_user_mail_address, user_mail_address_value

    !> The name of this machine, for use in constructing email addresses.
    !> If this is unallocated, Emacs uses `system_name'.  Port of
    !> startup.el mail-host-address.
    character(len=:), allocatable, public :: mail_host_address

    !> The email address of the current user.
    !> This defaults to either: the value of EMAIL environment variable; or
    !> user@host, using `user_login_name' and `mail_host_address' (or
    !> `system_name').  Port of startup.el user-mail-address (unallocated
    !> until initialized).
    character(len=:), allocatable, public :: user_mail_address

contains

    !> Give `user_mail_address' its standard value unless it was set (the
    !> custom-initialize-delay of startup.el:command-line).
    !> Port of part of startup.el:command-line.
    subroutine init_user_mail_address()
        integer :: n, st

        if (allocated(user_mail_address)) return
        call get_environment_variable("EMAIL", length=n, status=st)

        if (st == 0) then
            allocate (character(len=n) :: user_mail_address)
            if (n > 0) call get_environment_variable("EMAIL", user_mail_address)
        else if (allocated(mail_host_address)) then
            user_mail_address = user_login_name()//"@"//mail_host_address
        else
            user_mail_address = user_login_name()//"@"//system_name()
        end if
    end subroutine init_user_mail_address

    !> The value of `user_mail_address', initializing it first if needed.
    !> Not a port.
    function user_mail_address_value() result(address)
        character(len=:), allocatable :: address

        call init_user_mail_address()
        address = user_mail_address
    end function user_mail_address_value
end module startup_fm
