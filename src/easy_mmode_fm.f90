! easy_mmode_fm.f90 -- defining minor modes.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/easy-mmode.el.
!
! Copyright (C) 1997, 2000-2026 Free Software Foundation, Inc.
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

!> `define-minor-mode` for Fortran minor modes.  Upstream's macro
!> expands into a function; here define_minor_mode registers the mode
!> (its variable, lighter, keymap `MODE-map` and hooks) and set_minor_mode
!> does what that function does.  The mode's command is a small Fortran
!> procedure calling minor_mode_command:
!>
!>     subroutine cmd_my_mode(status)
!>         type(status_type), intent(out) :: status
!>         call minor_mode_command("my-mode", status)
!>     end subroutine cmd_my_mode
!>
!> registered with define_minor_mode("my-mode", " My", body=my_body)
!> and defcommand("my-mode", cmd_my_mode).
module easy_mmode_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil, nilp, t_value, make_keymap_ref
    use lread_fm, only: sym
    use data_fm, only: boundp, set, set_default, symbol_value, make_variable_buffer_local
    use eval_fm, only: run_hooks
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use keymap_fm, only: make_sparse_keymap, keymap_live_p, add_minor_mode_map
    use keyboard_fm, only: last_message
    use xdisp_fm, only: message, add_minor_mode_lighter
    implicit none(type, external)
    private

    public :: minor_mode_body_proc, define_minor_mode, set_minor_mode, minor_mode_command, &
              minor_mode_enabled_p, minor_mode_map, easy_mmode_pretty_mode_name

    abstract interface
        !> The body of a minor mode, run after its variable is set:
        !> `enable` is the new state.  A mode whose state is another
        !> variable (upstream's :variable) sets that variable here.
        subroutine minor_mode_body_proc(enable, status)
            import :: status_type
            implicit none(type, external)
            logical, intent(in) :: enable
            type(status_type), intent(out) :: status
        end subroutine minor_mode_body_proc
    end interface

    type :: minor_mode_type
        character(len=:), allocatable :: name, variable
        logical :: global = .false.
        logical :: own_variable = .true.
        type(handle_type) :: map
        procedure(minor_mode_body_proc), pointer, nopass :: body => null()
    end type minor_mode_type

    type(minor_mode_type), allocatable :: modes(:)
    integer :: nmodes = 0

contains

    !> Turn the symbol MODE into a string intended for the user.  Port of
    !> easy-mmode.el:easy-mmode-pretty-mode-name (without LIGHTER).
    function easy_mmode_pretty_mode_name(mode) result(name)
        character(len=*), intent(in) :: mode
        character(len=:), allocatable :: name, base
        integer :: i
        logical :: word_start
        base = mode
        if (len(base) >= 7) then
            if (base(1:7) == "toggle-") base = base(8:)
        end if
        if (len(base) >= 5) then
            if (base(len(base) - 4:) == "-mode") base = base(1:len(base) - 5)
        end if
        ! capitalize
        name = base
        word_start = .true.
        do i = 1, len(name)
            if (is_alnum(name(i:i))) then
                if (word_start) then
                    name(i:i) = upcase(name(i:i))
                else
                    name(i:i) = downcase(name(i:i))
                end if
                word_start = .false.
            else
                word_start = .true.
            end if
        end do
        i = index(name, "-Minor")
        if (i > 0) name = name(1:i - 1)//" minor"//name(i + 6:)
        name = name//" mode"
        if (len(name) >= 7) then
            if (name(1:7) == "Global-") name = "Global "//name(8:)
        end if
    end function easy_mmode_pretty_mode_name

    logical function is_alnum(c)
        character, intent(in) :: c
        is_alnum = (c >= "a" .and. c <= "z") .or. (c >= "A" .and. c <= "Z") .or. (c >= "0" .and. c <= "9")
    end function is_alnum

    character function upcase(c)
        character, intent(in) :: c
        upcase = c
        if (c >= "a" .and. c <= "z") upcase = achar(iachar(c) - 32)
    end function upcase

    character function downcase(c)
        character, intent(in) :: c
        downcase = c
        if (c >= "A" .and. c <= "Z") downcase = achar(iachar(c) + 32)
    end function downcase

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

    !> Define a new minor mode MODE.
    !> This defines the toggle command MODE and (by default) a control variable
    !> MODE (you can override this with VARIABLE).  MODE is buffer-local
    !> unless GLOBAL is true.  LIGHTER (if not empty) is shown in the mode
    !> line while the mode is on; it should begin with a space.
    !>
    !> BODY, if present, is run every time the mode is turned on or off,
    !> after the variable is set.  The keymap `MODE-map` is active while
    !> the mode is on, and the hooks `MODE-hook` and `MODE-on-hook` or
    !> `MODE-off-hook` run after BODY.  Port of
    !> easy-mmode.el:define-minor-mode (its command is defined by the
    !> caller, see the module documentation).
    subroutine define_minor_mode(mode, lighter, body, global, variable)
        character(len=*), intent(in) :: mode, lighter
        procedure(minor_mode_body_proc), optional :: body
        logical, intent(in), optional :: global
        character(len=*), intent(in), optional :: variable
        type(minor_mode_type), allocatable :: grown(:)
        type(status_type) :: st
        logical :: bound
        integer :: k
        character(len=:), allocatable :: hook
        k = find_mode(mode)
        if (k == 0) then
            if (.not. allocated(modes)) allocate (modes(16))
            if (nmodes == size(modes)) then
                allocate (grown(2*nmodes))
                grown(1:nmodes) = modes
                call move_alloc(grown, modes)
            end if
            nmodes = nmodes + 1
            k = nmodes
            modes(k)%name = mode
        end if
        modes(k)%variable = mode
        modes(k)%own_variable = .true.
        if (present(variable)) then
            modes(k)%variable = variable
            modes(k)%own_variable = variable == mode
        end if
        modes(k)%global = .false.
        if (present(global)) modes(k)%global = global
        modes(k)%body => null()
        if (present(body)) modes(k)%body => body
        if (modes(k)%own_variable) then
            call boundp(sym(mode), bound, st)
            if (.not. bound) call set(sym(mode), nil(), st)
            if (.not. modes(k)%global) call make_variable_buffer_local(sym(mode), st)
        end if
        if (len(lighter) > 0) call add_minor_mode_lighter(modes(k)%variable, lighter)
        if (.not. keymap_live_p(modes(k)%map)) then
            call make_sparse_keymap(modes(k)%map)
            call set(sym(mode//"-map"), make_keymap_ref(modes(k)%map), st)
            call add_minor_mode_map(modes(k)%variable, modes(k)%map)
        end if
        hook = mode//"-hook"
        call boundp(sym(hook), bound, st)
        if (.not. bound) call set(sym(hook), nil(), st)
    end subroutine define_minor_mode

    !> The keymap of minor mode MODE (`MODE-map`), or the null handle.
    function minor_mode_map(mode) result(h)
        character(len=*), intent(in) :: mode
        type(handle_type) :: h
        integer :: k
        h = null_handle()
        k = find_mode(mode)
        if (k > 0) h = modes(k)%map
    end function minor_mode_map

    !> Whether minor mode MODE is on (its variable is non-nil).
    logical function minor_mode_enabled_p(mode) result(on)
        character(len=*), intent(in) :: mode
        type(value_type) :: v
        type(status_type) :: st
        integer :: k
        on = .false.
        k = find_mode(mode)
        if (k == 0) return
        call symbol_value(sym(modes(k)%variable), v, st)
        on = st%ok() .and. .not. nilp(v)
    end function minor_mode_enabled_p

    !> Turn minor mode MODE on or off, as its function does: with ARG nil
    !> (or absent) turn it on, unless TOGGLE, then toggle it; with a
    !> number, turn it on if positive and off otherwise.  INTERACTIVE
    !> shows the "... enabled" message.  Port of the function defined by
    !> easy-mmode.el:define-minor-mode.
    recursive subroutine set_minor_mode(mode, status, arg, toggle, interactive)
        character(len=*), intent(in) :: mode
        type(status_type), intent(out) :: status
        type(value_type), intent(in), optional :: arg
        logical, intent(in), optional :: toggle, interactive
        logical :: enable, on, do_toggle, verbose, had_message
        character(len=:), allocatable :: before
        character(len=128) :: hooks(2)
        integer :: k
        status = status_type()
        k = find_mode(mode)
        if (k == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Undefined minor mode: "//mode)
            return
        end if
        do_toggle = .false.
        if (present(toggle)) do_toggle = toggle
        verbose = .false.
        if (present(interactive)) verbose = interactive
        enable = .true.
        if (present(arg)) then
            if (nilp(arg)) then
                if (do_toggle) enable = .not. minor_mode_enabled_p(mode)
            else
                enable = prefix_numeric_value(arg) >= 1
            end if
        else if (do_toggle) then
            enable = .not. minor_mode_enabled_p(mode)
        end if
        had_message = allocated(last_message)
        if (had_message) before = last_message
        if (modes(k)%own_variable) then
            if (modes(k)%global) then
                if (enable) then
                    call set_default(sym(mode), t_value(), status)
                else
                    call set_default(sym(mode), nil(), status)
                end if
            else if (enable) then
                call set(sym(mode), t_value(), status)
            else
                call set(sym(mode), nil(), status)
            end if
            if (.not. status%ok()) return
        end if
        if (associated(modes(k)%body)) then
            call modes(k)%body(enable, status)
            if (.not. status%ok()) return
        end if
        on = minor_mode_enabled_p(mode)
        ! The on/off hooks are here for backward compatibility only.
        hooks(1) = mode//"-hook"
        if (on) then
            hooks(2) = mode//"-on-hook"
        else
            hooks(2) = mode//"-off-hook"
        end if
        call run_hooks(hooks, status)
        if (.not. status%ok()) return
        if (.not. verbose) return
        ! Avoid overwriting a message shown by the body, but do overwrite
        ! previous messages.
        if (allocated(last_message)) then
            if (len(last_message) > 0) then
                if (.not. had_message) return
                if (last_message /= before) return
            end if
        end if
        if (on) then
            call message(easy_mmode_pretty_mode_name(mode)//" enabled"//local_text(k))
        else
            call message(easy_mmode_pretty_mode_name(mode)//" disabled"//local_text(k))
        end if
    end subroutine set_minor_mode

    function local_text(k) result(s)
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        if (modes(k)%global) then
            s = ""
        else
            s = " in current buffer"
        end if
    end function local_text

    !> Run minor mode MODE as its command: toggle it, or with a prefix
    !> argument turn it on if the argument is positive and off otherwise.
    recursive subroutine minor_mode_command(mode, status)
        character(len=*), intent(in) :: mode
        type(status_type), intent(out) :: status
        call set_minor_mode(mode, status, current_prefix_arg, toggle=.true., interactive=.true.)
    end subroutine minor_mode_command
end module easy_mmode_fm
