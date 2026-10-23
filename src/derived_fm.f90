! derived_fm.f90 -- defining derived major modes.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/derived.el.
!
! Copyright (C) 1993-1994, 1999, 2001-2026 Free Software Foundation, Inc.
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

!> `define-derived-mode` for Fortran major modes.  Upstream's macro
!> expands into a function; here a mode is a command procedure that
!> brackets its body with derived_mode_begin and derived_mode_end:
!>
!>     subroutine my_mode(status)
!>         type(status_type), intent(out) :: status
!>         logical :: saved
!>         call derived_mode_begin("my-mode", saved, status)
!>         if (.not. status%ok()) return
!>         call setq_local("fill-column", make_fixnum(60_int64), status)
!>         call derived_mode_end("my-mode", saved, status)
!>     end subroutine my_mode
!>
!> registered once with define_derived_mode("my-mode", "text-mode",
!> "My") and defcommand("my-mode", my_mode).
module derived_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil, make_keymap_ref
    use lread_fm, only: sym
    use data_fm, only: boundp, set, kill_all_local_variables
    use fns_fm, only: put
    use buffer_fm, only: buffers, cur
    use keymap_fm, only: make_sparse_keymap, keymap_live_p, keymap_parent, set_keymap_parent, use_local_map
    use subr_fm, only: delay_mode_hooks_begin, delay_mode_hooks_end, run_mode_hooks
    use eval_fm, only: funcall_noargs
    implicit none(type, external)
    private

    public :: define_derived_mode, derived_mode_begin, derived_mode_end, derived_mode_map, &
              derived_mode_hook_name, derived_mode_map_name

    type :: mode_type
        character(len=:), allocatable :: name, parent, mode_name
        type(handle_type) :: map, syntax_table
    end type mode_type

    type(mode_type), allocatable :: modes(:)
    integer :: nmodes = 0

contains

    !> Construct a variable name for the hook of MODE.  Port of
    !> derived.el:derived-mode-hook-name.
    function derived_mode_hook_name(mode) result(s)
        character(len=*), intent(in) :: mode
        character(len=:), allocatable :: s
        s = mode//"-hook"
    end function derived_mode_hook_name

    !> Construct a map name from a derived major mode name.  Port of
    !> derived.el:derived-mode-map-name.
    function derived_mode_map_name(mode) result(s)
        character(len=*), intent(in) :: mode
        character(len=:), allocatable :: s
        s = mode//"-map"
    end function derived_mode_map_name

    !> The index of MODE in the registry, or 0.
    integer function find_mode(mode) result(k)
        character(len=*), intent(in) :: mode
        integer :: i
        k = 0
        do i = 1, nmodes
            if (modes(i)%name == mode) then
                k = i
                return
            end if
        end do
    end function find_mode

    !> The keymap of major mode MODE (`MODE-map`), or the null handle.
    function derived_mode_map(mode) result(h)
        character(len=*), intent(in) :: mode
        type(handle_type) :: h
        integer :: k
        h = null_handle()
        k = find_mode(mode)
        if (k > 0) h = modes(k)%map
    end function derived_mode_map

    !> Create a new mode CHILD which is a variant of an existing mode PARENT
    !> (empty for none, upstream's nil).  NAME is a string that will appear
    !> in the mode line (e.g. "HTML").
    !>
    !> This defines the keymap `CHILD-map' (inheriting PARENT's keymap)
    !> and the hook `CHILD-hook'; SYNTAX_TABLE, if present, is the mode's
    !> syntax table (else it keeps the parent's).  Port of
    !> derived.el:define-derived-mode.
    subroutine define_derived_mode(child, parent, name, syntax_table)
        character(len=*), intent(in) :: child, parent, name
        type(handle_type), intent(in), optional :: syntax_table
        type(mode_type), allocatable :: grown(:)
        type(status_type) :: st
        logical :: bound
        integer :: k, p
        k = find_mode(child)
        if (k == 0) then
            if (.not. allocated(modes)) allocate (modes(16))
            if (nmodes == size(modes)) then
                allocate (grown(2*nmodes))
                grown(1:nmodes) = modes
                call move_alloc(grown, modes)
            end if
            nmodes = nmodes + 1
            k = nmodes
            modes(k)%name = child
        end if
        modes(k)%parent = parent
        modes(k)%mode_name = name
        if (present(syntax_table)) modes(k)%syntax_table = syntax_table
        if (.not. keymap_live_p(modes(k)%map)) then
            call make_sparse_keymap(modes(k)%map)
            call set(sym(derived_mode_map_name(child)), make_keymap_ref(modes(k)%map), st)
        end if
        if (len(parent) > 0) then
            call put(sym(child), sym("derived-mode-parent"), sym(parent), st)
            p = find_mode(parent)
            if (p > 0) then
                if (.not. keymap_live_p(keymap_parent(modes(k)%map))) then
                    call set_keymap_parent(modes(k)%map, modes(p)%map, st)
                end if
            end if
        end if
        call boundp(sym(derived_mode_hook_name(child)), bound, st)
        if (.not. bound) call set(sym(derived_mode_hook_name(child)), nil(), st)
    end subroutine define_derived_mode

    !> Start the body of major mode CHILD: run the parent mode with its
    !> mode hooks delayed (or kill all local variables), then set
    !> `major-mode`, `mode-name`, the local keymap and syntax table.
    !> SAVED is for derived_mode_end.  Port of the first part of
    !> define-derived-mode's expansion.
    recursive subroutine derived_mode_begin(child, saved, status)
        character(len=*), intent(in) :: child
        logical, intent(out) :: saved
        type(status_type), intent(out) :: status
        integer :: k
        status = status_type()
        saved = .false.
        k = find_mode(child)
        if (k == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Undefined major mode: "//child)
            return
        end if
        if (cur == 0) return
        call delay_mode_hooks_begin(saved)
        if (len(modes(k)%parent) > 0) then
            call funcall_noargs(sym(modes(k)%parent), status)
            if (.not. status%ok()) then
                call delay_mode_hooks_end(saved)
                return
            end if
        else
            call kill_all_local_variables()
        end if
        buffers(cur)%major_mode = child
        buffers(cur)%mode_name = modes(k)%mode_name
        call use_local_map(modes(k)%map, status)
        if (modes(k)%syntax_table%index /= 0) buffers(cur)%syntax_table = modes(k)%syntax_table
    end subroutine derived_mode_begin

    !> Finish the body of major mode CHILD: end the delay and run the
    !> mode hooks.  Port of the end of define-derived-mode's expansion.
    recursive subroutine derived_mode_end(child, saved, status)
        character(len=*), intent(in) :: child
        logical, intent(in) :: saved
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: hook
        call delay_mode_hooks_end(saved)
        hook = derived_mode_hook_name(child)
        call run_mode_hooks([hook], status)
    end subroutine derived_mode_end
end module derived_fm
