! simple_mail_mod.f90 -- composing mail with the chosen agent, a submodule
! of simple_mod.
! Ported from GNU Emacs 31.1 lisp/simple.el.
!
! Copyright (C) 1985-1987, 1993-2026 Free Software Foundation, Inc.
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

!> `compose_mail' (C-x m) and `compose_mail_other_window' (C-x 4 m): a
!> submodule because switching to the mail buffer in another window uses
!> window_mod, which uses simple_mod.
submodule(simple_mod) simple_mail_mod
    use window_mod, only: switch_to_buffer_other_window
    use buffer_mod, only: get_or_make_buffer
    implicit none(type, external)

contains

    module procedure compose_mail
    status = status_type()

    if (.not. associated(mail_user_agent)) then
        call set_error(status, GENERIC_ERROR_CODE, "Invalid value for `mail_user_agent'")
        return
    end if

    if (.not. associated(mail_user_agent%compose)) then
        call set_error(status, GENERIC_ERROR_CODE, "Invalid value for `mail_user_agent'")
        return
    end if

    if (present(switch_function)) then
        call mail_user_agent%compose(status, to, subject, continue, switch_function)
    else
        call mail_user_agent%compose(status, to, subject, continue)
    end if
    end procedure compose_mail

    module procedure compose_mail_other_window
    call compose_mail(status, to, subject, continue, switch_to_buffer_other_window_by_name)
    end procedure compose_mail_other_window

    !> `switch_to_buffer_other_window' on the buffer named BUFFER_NAME,
    !> made if needed (as upstream's does with a buffer name).  Not a port.
    recursive subroutine switch_to_buffer_other_window_by_name(buffer_name, status)
        character(len=*), intent(in) :: buffer_name
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer

        call get_or_make_buffer(buffer_name, buffer, status)
        if (status%ok()) call switch_to_buffer_other_window(buffer, status)
    end subroutine switch_to_buffer_other_window_by_name

    recursive subroutine execute_compose_mail(status)
        type(status_type), intent(out) :: status

        call compose_mail(status, continue=current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_compose_mail

    recursive subroutine execute_compose_mail_other_window(status)
        type(status_type), intent(out) :: status

        call compose_mail_other_window(status, continue=current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_compose_mail_other_window

    module procedure initialize_simple_mail
    commands%compose_mail%execute => execute_compose_mail
    commands%compose_mail_other_window%execute => execute_compose_mail_other_window
    end procedure initialize_simple_mail
end submodule simple_mail_mod
