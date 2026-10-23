! files_mode_fm.f90 -- choosing the major mode, a submodule of files_fm.
! Ported from GNU Emacs 31.1 lisp/files.el.
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

!> `normal-mode`, `set-auto-mode` and `auto-mode-alist`: a submodule
!> because the modes use files_fm's dependencies.
submodule(files_fm) files_mode_fm
    use lisp_fm, only: nil
    use data_fm, only: kill_all_local_variables, symbol_value
    use eval_fm, only: run_hooks, funcall_noargs
    use callint_fm, only: command_defined_p
    use search_fm, only: string_match, case_fold_search
    use editfns_fm, only: point_min, point_max, buffer_substring
    implicit none(type, external)

    !> An entry of auto-mode-alist.
    type :: auto_mode_type
        character(len=:), allocatable :: regexp, mode
    end type auto_mode_type

    !> auto-mode-alist, first entries first; set up on first use.
    type(auto_mode_type), allocatable :: auto_modes(:)
    integer :: n_auto_modes = 0

    !> How much of the buffer the -*- line is looked for in.
    integer(int64), parameter :: SPEC_LIMIT = 4096

contains

    !> Append an entry at the end of auto-mode-alist.
    subroutine append_auto_mode(regexp, mode)
        character(len=*), intent(in) :: regexp, mode
        type(auto_mode_type), allocatable :: grown(:)
        if (.not. allocated(auto_modes)) allocate (auto_modes(16))
        if (n_auto_modes == size(auto_modes)) then
            allocate (grown(2*n_auto_modes))
            grown(1:n_auto_modes) = auto_modes
            call move_alloc(grown, auto_modes)
        end if
        n_auto_modes = n_auto_modes + 1
        auto_modes(n_auto_modes)%regexp = regexp
        auto_modes(n_auto_modes)%mode = mode
    end subroutine append_auto_mode

    !> The default auto-mode-alist: files.el's entries for the modes
    !> that exist so far.
    subroutine init_auto_modes()
        if (allocated(auto_modes)) return
        call append_auto_mode("\.te?xt\'", "text-mode")
        call append_auto_mode("\.article\'", "text-mode")
        call append_auto_mode("\.letter\'", "text-mode")
        call append_auto_mode("\`/tmp/Re", "text-mode")
        call append_auto_mode("/Message[0-9]*\'", "text-mode")
        call append_auto_mode("\`/tmp/fol/", "text-mode")
    end subroutine init_auto_modes

    module procedure add_auto_mode
    type(auto_mode_type), allocatable :: grown(:)
    integer :: i
    call init_auto_modes()
    do i = 1, n_auto_modes
        if (auto_modes(i)%regexp == regexp .and. auto_modes(i)%mode == mode) return
    end do
    call append_auto_mode(regexp, mode)
    ! Move it to the front.
    allocate (grown(n_auto_modes))
    grown(1) = auto_modes(n_auto_modes)
    grown(2:n_auto_modes) = auto_modes(1:n_auto_modes - 1)
    auto_modes(1:n_auto_modes) = grown
    end procedure add_auto_mode

    module procedure file_name_sans_versions
    integer :: n, i
    base = name
    n = len(base)
    if (n == 0) return
    if (base(n:n) /= "~") return
    ! NAME.~N~ (numbered backups)
    i = index(base(1:n - 1), ".~", back=.true.)
    if (i > 0) then
        if (i + 2 <= n - 1) then
            if (verify(base(i + 2:n - 1), "0123456789") == 0) then
                base = base(1:i - 1)
                return
            end if
        end if
    end if
    ! NAME~ and NAME.~~
    base = base(1:n - 1)
    end procedure file_name_sans_versions

    pure function downcase(s) result(d)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: d
        integer :: i
        d = s
        do i = 1, len(s)
            if (s(i:i) >= "A" .and. s(i:i) <= "Z") d(i:i) = achar(iachar(s(i:i)) + 32)
        end do
    end function downcase

    !> S without leading and trailing spaces and tabs.
    pure function strip(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: a, b
        a = verify(s, " "//char(9))
        b = verify(s, " "//char(9), back=.true.)
        if (a == 0) then
            t = ""
        else
            t = s(a:b)
        end if
    end function strip

    module procedure auto_mode_spec
    character(len=:), allocatable :: line, rest, spec, lower, value
    integer :: a, nl, i, j, k
    logical :: skip
    mode = ""
    ! (skip-chars-forward " \t\n")
    a = verify(text, " "//char(9)//char(10))
    if (a == 0) return
    nl = index(text(a:), char(10))
    if (nl == 0) then
        line = text(a:)
    else
        line = text(a:a + nl - 2)
    end if
    ! If the file begins with "#!" or '\" (file-auto-mode-skip), look in
    ! the first two lines.
    skip = .false.
    if (len(line) >= 2) skip = line(1:2) == "#!"
    if (len(line) >= 3) skip = skip .or. line(1:3) == "'\"""
    i = index(line, "-*-")
    if (i == 0 .and. skip .and. nl > 0) then
        line = text(a + nl:)
        nl = index(line, char(10))
        if (nl > 0) line = line(1:nl - 1)
        i = index(line, "-*-")
    end if
    if (i == 0) return
    rest = line(i + 3:)
    j = index(rest, "-*-")
    if (j == 0) return
    spec = strip(rest(1:j - 1))
    if (index(spec, ":") == 0) then
        ! Simple -*-MODE-*- case.
        if (len(spec) > 0) mode = downcase(spec)//"-mode"
        return
    end if
    ! The first `mode:' entry: at the start or after [ \t;].
    lower = downcase(spec)
    k = 0
    if (len(lower) >= 5) then
        if (lower(1:5) == "mode:") k = 1
    end if
    if (k == 0) then
        do i = 2, len(lower) - 4
            if (lower(i:i + 4) == "mode:" .and. index(" "//char(9)//";", lower(i - 1:i - 1)) > 0) then
                k = i
                exit
            end if
        end do
    end if
    if (k == 0) return
    value = spec(k + 5:)
    j = index(value, ";")
    if (j > 0) value = value(1:j - 1)
    value = strip(value)
    if (len(value) > 0) mode = downcase(value)//"-mode"
    end procedure auto_mode_spec

    !> Apply MODE.  Port of files.el:set-auto-mode-0.  Returns whether it
    !> was applied (or kept).
    recursive logical function set_auto_mode_0(mode, keep_mode_if_same, status) result(done)
        character(len=*), intent(in) :: mode
        logical, intent(in) :: keep_mode_if_same
        type(status_type), intent(out) :: status
        status = status_type()
        done = .false.
        if (keep_mode_if_same .and. allocated(buffers(cur)%major_mode)) then
            if (buffers(cur)%major_mode == mode) then
                done = .true.
                return
            end if
        end if
        if (.not. command_defined_p(sym(mode))) then
            call message("Ignoring unknown mode `"//mode//"'")
            return
        end if
        call funcall_noargs(sym(mode), status)
        done = status%ok()
    end function set_auto_mode_0

    !> The mode of the first auto-mode-alist entry matching NAME, or "".
    !> Port of files.el:set-auto-mode--find-matching-alist-entry (no
    !> (REGEXP MODE t) entries).
    function find_matching_entry(name) result(mode)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: mode
        logical :: saved
        integer :: pass, i
        type(status_type) :: st
        mode = ""
        saved = case_fold_search
        do pass = 1, 2
            ! First match case-sensitively, then (auto-mode-case-fold)
            ! case-insensitively.
            case_fold_search = pass == 2
            do i = 1, n_auto_modes
                if (string_match(auto_modes(i)%regexp, name, st, inhibit_modify=.true.) > 0) then
                    mode = auto_modes(i)%mode
                    case_fold_search = saved
                    return
                end if
            end do
        end do
        case_fold_search = saved
    end function find_matching_entry

    module procedure set_auto_mode
    character(len=:), allocatable :: text, mode, name
    logical :: keep
    status = status_type()
    if (cur == 0) return
    keep = .false.
    if (present(keep_mode_if_same)) keep = keep_mode_if_same
    ! Look for -*-MODENAME-*- or -*- ... mode: MODENAME; ... -*-
    call buffer_substring(point_min(), min(point_max(), point_min() + SPEC_LIMIT), text, status)
    if (.not. status%ok()) return
    mode = auto_mode_spec(text)
    if (len(mode) > 0) then
        if (set_auto_mode_0(mode, keep, status)) return
        if (.not. status%ok()) return
    end if
    ! Next compare the filename against the entries in auto-mode-alist.
    call init_auto_modes()
    if (.not. allocated(buffers(cur)%filename)) return
    name = file_name_sans_versions(buffers(cur)%filename)
    mode = find_matching_entry(name)
    if (len(mode) > 0) then
        if (set_auto_mode_0(mode, keep, status)) return
    end if
    end procedure set_auto_mode

    module procedure normal_mode
    type(value_type) :: v
    type(status_type) :: st
    logical :: delaying
    character(len=40), parameter :: HOOKS(2) = [character(len=40) :: &
                                                "change-major-mode-after-body-hook", "after-change-major-mode-hook"]
    status = status_type()
    if (cur == 0) return
    ! FIND_FILE only matters for file-local variables, not yet supported.
    if (present(find_file)) continue
    call kill_all_local_variables()
    call symbol_value(sym("delay-mode-hooks"), v, st)
    delaying = st%ok() .and. .not. nilp(v)
    if (.not. delaying) then
        call run_hooks(HOOKS, status)
        if (.not. status%ok()) return
    end if
    ! (with-demoted-errors "File mode specification error: %S" ...)
    call set_auto_mode(st)
    if (.not. st%ok()) call message("File mode specification error: "//format_error(st))
    end procedure normal_mode
end submodule files_mode_fm
