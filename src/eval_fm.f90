! eval_fm.f90 -- running hooks.
! Ported from GNU Emacs 31.1 src/eval.c.
!
! Copyright (C) 1985-1987, 1993-1995, 1999-2026 Free Software Foundation,
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

!> Running hooks.  There is no evaluator: a hook is a variable whose
!> value is a list of symbols naming Fortran procedures registered with
!> `defcommand` (or one such symbol), and running it calls them with no
!> arguments.
module eval_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use lisp_fm, only: value_type, nil, nilp, symbolp, consp, list_length, list_elt, xsymbol, SYM_T
    use lread_fm, only: sym
    use data_fm, only: symbol_value, default_value
    use callint_fm, only: call_interactively, current_prefix_arg
    implicit none(type, external)
    private

    public :: run_hooks, run_hook, funcall_noargs

contains

    !> Call function FN with no arguments (and no prefix argument).
    !> Port of eval.c:funcall_nil.
    recursive subroutine funcall_noargs(fn, status)
        type(value_type), intent(in) :: fn
        type(status_type), intent(out) :: status
        type(value_type) :: saved
        saved = current_prefix_arg
        current_prefix_arg = nil()
        call call_interactively(fn, status)
        current_prefix_arg = saved
    end subroutine funcall_noargs

    !> Run each function of hook variable HOOK, stopping at the first
    !> error.  A `t` in a buffer-local value means to run the default
    !> value's functions too.  Port of eval.c:run_hook (via
    !> run_hook_with_args).
    recursive subroutine run_hook(hook, status)
        character(len=*), intent(in) :: hook
        type(status_type), intent(out) :: status
        type(value_type) :: val, fn, global
        type(status_type) :: st
        integer(int64) :: i, j
        status = status_type()
        call symbol_value(sym(hook), val, st)
        if (.not. st%ok()) return
        if (nilp(val)) return
        if (.not. consp(val)) then
            call funcall_noargs(val, status)
            return
        end if
        do i = 1, list_length(val)
            fn = list_elt(val, i)
            if (symbolp(fn) .and. xsymbol(fn) == SYM_T) then
                ! t indicates this hook has a local binding; it means to
                ! run the global binding too.
                call default_value(sym(hook), global, st)
                if (.not. st%ok()) cycle
                if (nilp(global)) cycle
                if (.not. consp(global)) then
                    call funcall_noargs(global, status)
                else
                    do j = 1, list_length(global)
                        fn = list_elt(global, j)
                        ! In a global value, t should not occur.  If it
                        ! does, we must ignore it to avoid an endless loop.
                        if (symbolp(fn) .and. xsymbol(fn) == SYM_T) cycle
                        call funcall_noargs(fn, status)
                        if (.not. status%ok()) return
                    end do
                end if
            else
                call funcall_noargs(fn, status)
            end if
            if (.not. status%ok()) return
        end do
    end subroutine run_hook

    !> Run each hook in HOOKS.
    !> Each argument should be a symbol, a hook variable.
    !> These symbols are processed in the order specified.
    !> If a hook symbol has a non-nil value, that value may be a function
    !> or a list of functions to be called to run the hook.
    !> If the value is a function, it is called with no arguments.
    !> If it is a list, the elements are called, in order, with no arguments.
    !>
    !> Major modes should not use this function directly to run their mode
    !> hook; they should use `run-mode-hooks' instead.
    !>
    !> Do not use `make-local-variable' to make a hook variable buffer-local.
    !> Instead, use `add-hook' and specify t for the LOCAL argument.
    !> Port of eval.c:Frun_hooks.
    recursive subroutine run_hooks(hooks, status)
        character(len=*), intent(in) :: hooks(:)
        type(status_type), intent(out) :: status
        integer :: i
        status = status_type()
        do i = 1, size(hooks)
            call run_hook(trim(hooks(i)), status)
            if (.not. status%ok()) return
        end do
    end subroutine run_hooks
end module eval_fm
