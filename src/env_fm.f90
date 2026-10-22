! env_fm.f90 -- functions to manipulate environment variables.
! Ported from GNU Emacs 31.1 lisp/env.el.
!
! Copyright (C) 1991-2026 Free Software Foundation, Inc.
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

!> Reading environment variables and substituting them into strings.
!> The process environment is read directly (there is no
!> `process-environment` list yet).
module env_fm
    implicit none(type, external)
    private

    public :: getenv, substitute_env_vars

contains

    !> The value of environment variable `variable`; `found` is false if
    !> it is not set.  Port of env.el:getenv (no FRAME argument).
    subroutine getenv(variable, value, found)
        character(len=*), intent(in) :: variable
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out) :: found
        integer :: length, stat
        call get_environment_variable(variable, length=length, status=stat)
        found = stat == 0
        allocate (character(len=max(length, 0)) :: value)
        if (found .and. length > 0) call get_environment_variable(variable, value)
    end subroutine getenv

    !> `string` with $FOO and ${FOO} replaced by environment variables and
    !> $$ by $.  References to unset variables are left unchanged with
    !> `keep_undefined`, else replaced by "".  Port of
    !> env.el:substitute-env-vars (WHEN-UNDEFINED nil or t).
    function substitute_env_vars(string, keep_undefined) result(out)
        character(len=*), intent(in) :: string
        logical, intent(in) :: keep_undefined
        character(len=:), allocatable :: out
        character(len=:), allocatable :: value
        integer :: i, j, rbrace
        logical :: found
        out = ""
        i = 1
        do while (i <= len(string))
            if (string(i:i) /= "$" .or. i == len(string)) then
                out = out//string(i:i)
                i = i + 1
                cycle
            end if
            ! env--substitute-vars-regexp: $NAME, ${NAME} or $$
            if (string(i + 1:i + 1) == "$") then
                out = out//"$"
                i = i + 2
            else if (string(i + 1:i + 1) == "{") then
                rbrace = index(string(i + 2:), "}")
                if (rbrace <= 1 .or. index(string(i + 2:i + rbrace), "{") > 0) then
                    out = out//"$"
                    i = i + 1
                    cycle
                end if
                call getenv(string(i + 2:i + rbrace), value, found)
                if (found .or. .not. keep_undefined) then
                    out = out//value
                else
                    out = out//string(i:i + rbrace + 1)
                end if
                i = i + rbrace + 2
            else
                j = i + 1
                do while (j <= len(string))
                    if (.not. name_char(string(j:j))) exit
                    j = j + 1
                end do
                if (j == i + 1) then
                    out = out//"$"
                    i = i + 1
                    cycle
                end if
                call getenv(string(i + 1:j - 1), value, found)
                if (found .or. .not. keep_undefined) then
                    out = out//value
                else
                    out = out//string(i:j - 1)
                end if
                i = j
            end if
        end do
    contains

        !> [[:alnum:]_] (ASCII).
        pure logical function name_char(c)
            character, intent(in) :: c
            name_char = verify(c, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_") == 0
        end function name_char
    end function substitute_env_vars
end module env_fm
