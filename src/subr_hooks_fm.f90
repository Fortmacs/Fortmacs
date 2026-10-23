! subr_hooks_fm.f90 -- hooks and mode hooks, a submodule of subr_fm.
! Ported from GNU Emacs 31.1 lisp/subr.el.
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> `add-hook`, `remove-hook`, `run-mode-hooks`, `delay-mode-hooks` and
!> the derived-mode predicates: a submodule because they use eval_fm
!> and buffer-local variables.
submodule(subr_fm) subr_hooks_fm
    use lisp_fm, only: t_value, nilp, consp, symbolp, make_list, list_elements, xsymbol, symbols, SYM_T, list, &
                       make_fixnum
    use data_fm, only: boundp, default_boundp, set_default, default_value, make_local_variable, &
                       local_variable_p, local_variable_if_set_p, kill_local_variable, setq_local, value_eq
    use fns_fm, only: get, put
    use eval_fm, only: run_hooks
    use buffer_fm, only: buffers, cur
    implicit none(type, external)

    !> Longest hook name passed to run_hooks.
    integer, parameter :: HOOK_NAME_LEN = 128

contains

    !> The functions in hook value V as an array (a single function
    !> becomes a list of one).
    subroutine hook_functions(v, fns)
        type(value_type), intent(in) :: v
        type(value_type), allocatable, intent(out) :: fns(:)
        if (nilp(v)) then
            allocate (fns(0))
        else if (consp(v)) then
            call list_elements(v, fns)
        else
            allocate (fns(1))
            fns(1) = v
        end if
    end subroutine hook_functions

    !> Whether T is among FNS.
    logical function has_t(fns)
        type(value_type), intent(in) :: fns(:)
        integer :: i
        has_t = .false.
        do i = 1, size(fns)
            if (symbolp(fns(i)) .and. xsymbol(fns(i)) == SYM_T) has_t = .true.
        end do
    end function has_t

    !> The index of FN in FNS, or 0.
    integer function find_fn(fns, fn) result(k)
        type(value_type), intent(in) :: fns(:), fn
        integer :: i
        k = 0
        do i = 1, size(fns)
            if (value_eq(fns(i), fn)) then
                k = i
                return
            end if
        end do
    end function find_fn

    !> The depth recorded for FN in DEPTHS, a list of (FUNCTION DEPTH).
    integer(int64) function depth_of(depths, fn) result(d)
        type(value_type), intent(in) :: depths(:), fn
        integer :: i
        d = 0
        do i = 1, size(depths)
            if (value_eq(list_elt(depths(i), 1_int64), fn)) then
                d = xfixnum(list_elt(depths(i), 2_int64))
                return
            end if
        end do
    end function depth_of

    module procedure add_hook
    type(value_type) :: hs, fs, v, prop
    type(value_type), allocatable :: fns(:), grown(:), depths(:), dgrown(:)
    type(status_type) :: st
    logical :: loc, bound
    integer :: d, i, j, n
    type(value_type) :: tmp
    status = status_type()
    hs = sym(hook)
    fs = sym(function)
    call boundp(hs, bound, st)
    if (.not. bound) call set(hs, nil(), st)
    if (.not. default_boundp(hs)) call set_default(hs, nil(), st)
    d = 0
    if (present(depth)) d = depth
    loc = .false.
    if (present(local)) loc = local
    if (loc) then
        if (.not. local_variable_if_set_p(hs)) then
            call make_local_variable(hs, status)
            if (.not. status%ok()) return
            call set(hs, list(t_value()), status)
            if (.not. status%ok()) return
        end if
    else if (local_variable_if_set_p(hs)) then
        call symbol_value(hs, v, st)
        call hook_functions(v, fns)
        if (.not. (consp(v) .and. has_t(fns))) loc = .true.
    end if
    if (loc) then
        call symbol_value(hs, v, st)
    else
        call default_value(hs, v, st)
    end if
    call hook_functions(v, fns)
    if (find_fn(fns, fs) > 0) return
    ! Depths, as a list of (FUNCTION DEPTH) on the hook's
    ! `hook--depth-alist' property.
    call get(hs, sym("hook--depth-alist"), prop, st)
    call hook_functions(prop, depths)
    if (d /= 0) then
        n = size(depths)
        allocate (dgrown(n + 1))
        dgrown(1:n) = depths
        dgrown(n + 1) = list(fs, make_fixnum(int(d, int64)))
        call move_alloc(dgrown, depths)
        call put(hs, sym("hook--depth-alist"), make_list(depths), st)
    end if
    n = size(fns)
    allocate (grown(n + 1))
    if (d > 0) then
        grown(1:n) = fns
        grown(n + 1) = fs
    else
        grown(1) = fs
        grown(2:n + 1) = fns
    end if
    if (size(depths) > 0) then
        ! A stable insertion sort by depth.
        do i = 2, n + 1
            tmp = grown(i)
            j = i - 1
            do while (j >= 1)
                if (depth_of(depths, grown(j)) <= depth_of(depths, tmp)) exit
                grown(j + 1) = grown(j)
                j = j - 1
            end do
            grown(j + 1) = tmp
        end do
    end if
    if (loc) then
        call set(hs, make_list(grown), status)
    else
        call set_default(hs, make_list(grown), status)
    end if
    end procedure add_hook

    module procedure remove_hook
    type(value_type) :: hs, fs, v
    type(value_type), allocatable :: fns(:)
    type(status_type) :: st
    logical :: loc, bound
    integer :: k
    status = status_type()
    hs = sym(hook)
    fs = sym(function)
    call boundp(hs, bound, st)
    if (.not. bound) call set(hs, nil(), st)
    if (.not. default_boundp(hs)) call set_default(hs, nil(), st)
    loc = .false.
    if (present(local)) loc = local
    if (loc .and. .not. local_variable_p(hs)) return
    if (local_variable_p(hs)) then
        call symbol_value(hs, v, st)
        call hook_functions(v, fns)
        if (.not. (consp(v) .and. has_t(fns))) loc = .true.
    end if
    if (loc) then
        call symbol_value(hs, v, st)
    else
        call default_value(hs, v, st)
    end if
    call hook_functions(v, fns)
    k = find_fn(fns, fs)
    if (k == 0) return
    fns = [fns(1:k - 1), fns(k + 1:)]
    if (.not. loc) then
        call set_default(hs, make_list(fns), status)
    else if (size(fns) == 1 .and. has_t(fns)) then
        call kill_local_variable(hs, status)
    else
        call set(hs, make_list(fns), status)
    end if
    end procedure remove_hook

    module procedure delay_mode_hooks_begin
    type(value_type) :: v
    type(status_type) :: st
    call put(sym("delay-mode-hooks"), sym("permanent-local"), t_value(), st)
    call symbol_value(sym("delay-mode-hooks"), v, st)
    saved = st%ok() .and. .not. nilp(v)
    call setq_local("delay-mode-hooks", t_value(), st)
    end procedure delay_mode_hooks_begin

    module procedure delay_mode_hooks_end
    type(status_type) :: st
    if (saved) then
        call set(sym("delay-mode-hooks"), t_value(), st)
    else
        call set(sym("delay-mode-hooks"), nil(), st)
    end if
    end procedure delay_mode_hooks_end

    module procedure run_mode_hooks
    type(value_type) :: v
    type(value_type), allocatable :: delayed(:), grown(:)
    character(len=HOOK_NAME_LEN), allocatable :: names(:)
    type(status_type) :: st
    integer :: i, n
    status = status_type()
    call symbol_value(sym("delayed-mode-hooks"), v, st)
    if (.not. st%ok()) v = nil()
    call hook_functions(v, delayed)
    call symbol_value(sym("delay-mode-hooks"), v, st)
    if (st%ok() .and. .not. nilp(v)) then
        ! Delaying case.
        n = size(delayed)
        allocate (grown(n + size(hooks)))
        do i = 1, size(hooks)
            grown(size(hooks) - i + 1) = sym(trim(hooks(i)))
        end do
        grown(size(hooks) + 1:) = delayed
        call setq_local("delayed-mode-hooks", make_list(grown), status)
        return
    end if
    ! Normal case, just run the hook as before plus any delayed hooks.
    n = size(delayed)
    allocate (names(1 + n + size(hooks)))
    names(1) = "change-major-mode-after-body-hook"
    do i = 1, n
        names(1 + i) = symbols(xsymbol(delayed(n - i + 1)))%name
    end do
    do i = 1, size(hooks)
        names(1 + n + i) = hooks(i)
    end do
    call setq_local("delayed-mode-hooks", nil(), st)
    call run_hooks(names, status)
    if (.not. status%ok()) return
    call run_hooks([character(len=HOOK_NAME_LEN) :: "after-change-major-mode-hook"], status)
    end procedure run_mode_hooks

    module procedure provided_mode_derived_p
    character(len=:), allocatable :: m
    type(value_type) :: parent
    type(status_type) :: st
    integer :: i, depth
    provided_mode_derived_p = .false.
    m = mode
    do depth = 1, 100
        do i = 1, size(modes)
            if (m == trim(modes(i))) then
                provided_mode_derived_p = .true.
                return
            end if
        end do
        call get(sym(m), sym("derived-mode-parent"), parent, st)
        if (.not. symbolp(parent) .or. nilp(parent)) return
        m = symbols(xsymbol(parent))%name
    end do
    end procedure provided_mode_derived_p

    module procedure derived_mode_p
    derived_mode_p = .false.
    if (cur == 0) return
    if (allocated(buffers(cur)%major_mode)) then
        derived_mode_p = provided_mode_derived_p(buffers(cur)%major_mode, modes)
    else
        derived_mode_p = provided_mode_derived_p("fundamental-mode", modes)
    end if
    end procedure derived_mode_p
end submodule subr_hooks_fm
