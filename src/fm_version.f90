! fm_version.f90 -- version identification for Fortmacs.
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

!> Version constants.  Not a port: Fortmacs has no upstream equivalent
!> of its own version number, only the Emacs release it tracks.
module fm_version
   implicit none(type, external)
   private

   public :: fortmacs_version, upstream_version, version_string

   !> Fortmacs release number.
   character(len=*), parameter :: fortmacs_version = "0.0.0"
   !> GNU Emacs release this port tracks (tag emacs-31.1).
   character(len=*), parameter :: upstream_version = "31.1"

contains

   !> One-line version banner, as printed by `fortmacs --version`.
   pure function version_string() result(s)
      character(len=:), allocatable :: s

      s = "Fortmacs "//fortmacs_version//" (port of GNU Emacs "// &
          upstream_version//")"
   end function version_string

end module fm_version
