! data_buffer_fm.f90 -- buffer-local variables, a submodule of data_fm.
! Ported from GNU Emacs 31.1 src/data.c and src/buffer.c.
!
! Copyright (C) 1985-1986, 1988, 1993-1995, 1997-2026 Free Software
! Foundation, Inc.
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

!> Buffer-local variables (data.c's SYMBOL_LOCALIZED and
!> buffer.c's local_var_alist) and the variables forwarded to Fortran
!> settings (data.c's SYMBOL_FORWARDED).
!>
!> The value of a variable is always the current buffer's.  When a
!> buffer has its own value for a variable, the default is kept in the
!> symbol (`default_val`) while that buffer is current, and the local
!> value in the buffer (`local_values`) while it is not; the swap
!> happens in set_buffer_internal.  Variables forwarded to procedure
!> pointers (auto-fill-function, indent-line-function) keep the pointers
!> themselves in this submodule, so a procedure without a name survives
!> a swap; as values they are known by name.
!>
!> A submodule because the Fortran settings live in modules that use
!> data_fm.
submodule(data_fm) data_buffer_fm
    use lisp_fm, only: t_value, nilp, make_fixnum, make_string, make_symbol_ref, make_list, xfixnum, xstring
    use lread_fm, only: intern, sym
    use handle_fm, only: null_handle
    use buffer_fm, only: buffers, cur, swap_buffer_locals_hook, fill_column, left_margin, &
                         auto_fill_function, auto_fill_proc, buffer_live_p, buffer_handle, transient_mark_mode
    use xdisp_fm, only: tab_width
    use indent_fm, only: indent_tabs_mode, standard_indent, indent_line_function, indent_line_proc
    use search_fm, only: case_fold_search
    use paragraphs_fm, only: paragraph_start, paragraph_separate, sentence_end, &
                             sentence_end_double_space, init_paragraph_variables
    use fill_fm, only: adaptive_fill_mode
    use simple_fm, only: fill_prefix, auto_fill_inhibit_regexp, do_auto_fill
    use fns_fm, only: get
    implicit none(type, external)

    !> Kinds of forwarded variable.
    integer, parameter :: K_INT64 = 1, K_INT = 2, K_BOOL = 3, K_STRING = 4, K_AUTO_FILL = 5, &
                          K_INDENT_LINE = 6, K_SLOT = 7

    !> The variables forwarded to Fortran settings; slot variables are
    !> per-buffer fields of buffer_type (always local, like buffer.c's
    !> slots with a local flag of -1).
    integer, parameter :: NFWD = 19
    character(len=*), parameter :: FWD_NAMES(NFWD) = [character(len=24) :: &
        "fill-column", "left-margin", "tab-width", "indent-tabs-mode", "case-fold-search", &
        "fill-prefix", "paragraph-start", "paragraph-separate", "sentence-end", &
        "sentence-end-double-space", "adaptive-fill-mode", "auto-fill-inhibit-regexp", &
        "standard-indent", "auto-fill-function", "indent-line-function", "transient-mark-mode", &
        "major-mode", "mode-name", "buffer-read-only"]
    integer, parameter :: FWD_KIND(NFWD) = [K_INT64, K_INT64, K_INT, K_BOOL, K_BOOL, &
                                            K_STRING, K_STRING, K_STRING, K_STRING, &
                                            K_BOOL, K_BOOL, K_STRING, &
                                            K_INT64, K_AUTO_FILL, K_INDENT_LINE, K_BOOL, &
                                            K_SLOT, K_SLOT, K_SLOT]
    !> DEFVAR_PER_BUFFER, `make-variable-buffer-local` or `:local t`:
    !> local when set.
    logical, parameter :: FWD_PER_BUFFER(NFWD) = [.true., .true., .true., .false., .true., &
                                                  .true., .false., .false., .false., &
                                                  .false., .false., .false., &
                                                  .false., .true., .false., .false., &
                                                  .true., .true., .true.]

    !> Saved procedure pointers: the default while the current buffer
    !> has a local value, or a buffer's local value while it is not
    !> current.
    type :: procs_type
        procedure(auto_fill_proc), pointer, nopass :: auto_fill => null()
        procedure(indent_line_proc), pointer, nopass :: indent_line => null()
    end type procs_type

    type(procs_type) :: default_procs
    !> Indexed by buffer slot.
    type(procs_type), allocatable :: saved_procs(:)

contains

    ! ------------------------------------------------------------------
    ! Forwarding
    ! ------------------------------------------------------------------

    !> The forwarding table index of symbol ID, or -1.
    integer function resolve(id) result(k)
        integer(int64), intent(in) :: id
        integer :: i
        if (symbols(id)%fwd == 0) then
            symbols(id)%fwd = -1
            do i = 1, NFWD
                if (symbols(id)%name == trim(FWD_NAMES(i))) then
                    symbols(id)%fwd = i
                    if (FWD_PER_BUFFER(i)) symbols(id)%local_if_set = .true.
                    exit
                end if
            end do
        end if
        k = symbols(id)%fwd
    end function resolve

    !> The kind of symbol ID: 0 for a plain variable.
    integer function kind_of(id) result(kind)
        integer(int64), intent(in) :: id
        integer :: k
        kind = 0
        k = resolve(id)
        if (k > 0) kind = FWD_KIND(k)
    end function kind_of

    !> The name of an auto-fill-function as a value.
    function auto_fill_value(p) result(v)
        procedure(auto_fill_proc), pointer, intent(in) :: p
        type(value_type) :: v
        if (.not. associated(p)) then
            v = nil()
        else if (associated(p, do_auto_fill)) then
            v = sym("do-auto-fill")
        else
            v = t_value()
        end if
    end function auto_fill_value

    !> The name of an indent-line-function as a value (null is the
    !> default, `indent-relative`).
    function indent_line_value(p) result(v)
        procedure(indent_line_proc), pointer, intent(in) :: p
        type(value_type) :: v
        if (.not. associated(p)) then
            v = sym("indent-relative")
        else
            v = t_value()
        end if
    end function indent_line_value

    !> The procedure named by V for auto-fill-function.
    subroutine auto_fill_from(v, p, status)
        type(value_type), intent(in) :: v
        procedure(auto_fill_proc), pointer, intent(inout) :: p
        type(status_type), intent(out) :: status
        status = status_type()
        if (nilp(v)) then
            p => null()
        else if (symbolp(v) .and. symbols(xsymbol(v))%name == "do-auto-fill") then
            p => do_auto_fill
        else
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "functionp")
        end if
    end subroutine auto_fill_from

    !> The procedure named by V for indent-line-function.
    subroutine indent_line_from(v, p, status)
        type(value_type), intent(in) :: v
        procedure(indent_line_proc), pointer, intent(inout) :: p
        type(status_type), intent(out) :: status
        status = status_type()
        if (symbolp(v) .and. .not. nilp(v)) then
            if (symbols(xsymbol(v))%name == "indent-relative") then
                p => null()
                return
            end if
        end if
        call set_error(status, FM_WRONG_TYPE_ARGUMENT, "functionp")
    end subroutine indent_line_from

    !> A string or nil.
    function string_or_nil(s) result(v)
        character(len=:), allocatable, intent(in) :: s
        type(value_type) :: v
        if (allocated(s)) then
            v = make_string(s)
        else
            v = nil()
        end if
    end function string_or_nil

    !> Store V, a string or nil, in S.
    subroutine set_string_or_nil(v, s, status)
        type(value_type), intent(in) :: v
        character(len=:), allocatable, intent(inout) :: s
        type(status_type), intent(out) :: status
        status = status_type()
        if (nilp(v)) then
            if (allocated(s)) deallocate (s)
        else if (stringp(v)) then
            s = xstring(v)
        else
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "stringp")
        end if
    end subroutine set_string_or_nil

    function bool_value(b) result(v)
        logical, intent(in) :: b
        type(value_type) :: v
        if (b) then
            v = t_value()
        else
            v = nil()
        end if
    end function bool_value

    !> The value of forwarded variable K in buffer B (B matters only for
    !> slot variables; the others read the current value).
    subroutine fwd_get(k, b, v)
        integer, intent(in) :: k
        integer(int64), intent(in) :: b
        type(value_type), intent(out) :: v
        select case (trim(FWD_NAMES(k)))
        case ("fill-column")
            v = make_fixnum(fill_column)
        case ("left-margin")
            v = make_fixnum(left_margin)
        case ("tab-width")
            v = make_fixnum(int(tab_width, int64))
        case ("indent-tabs-mode")
            v = bool_value(indent_tabs_mode)
        case ("case-fold-search")
            v = bool_value(case_fold_search)
        case ("fill-prefix")
            v = string_or_nil(fill_prefix)
        case ("paragraph-start")
            call init_paragraph_variables()
            v = string_or_nil(paragraph_start)
        case ("paragraph-separate")
            call init_paragraph_variables()
            v = string_or_nil(paragraph_separate)
        case ("sentence-end")
            v = string_or_nil(sentence_end)
        case ("sentence-end-double-space")
            v = bool_value(sentence_end_double_space)
        case ("adaptive-fill-mode")
            v = bool_value(adaptive_fill_mode)
        case ("auto-fill-inhibit-regexp")
            v = string_or_nil(auto_fill_inhibit_regexp)
        case ("standard-indent")
            v = make_fixnum(standard_indent)
        case ("auto-fill-function")
            v = auto_fill_value(auto_fill_function)
        case ("indent-line-function")
            v = indent_line_value(indent_line_function)
        case ("transient-mark-mode")
            v = bool_value(transient_mark_mode)
        case ("major-mode")
            if (b > 0 .and. allocated(buffers(b)%major_mode)) then
                v = sym(buffers(b)%major_mode)
            else
                v = sym("fundamental-mode")
            end if
        case ("mode-name")
            if (b > 0 .and. allocated(buffers(b)%mode_name)) then
                v = make_string(buffers(b)%mode_name)
            else
                v = make_string("Fundamental")
            end if
        case ("buffer-read-only")
            v = nil()
            if (b > 0) v = bool_value(buffers(b)%read_only)
        case default
            v = nil()
        end select
    end subroutine fwd_get

    !> Set forwarded variable K (of the current buffer, for slots).
    subroutine fwd_set(k, v, status)
        integer, intent(in) :: k
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status
        status = status_type()
        select case (FWD_KIND(k))
        case (K_INT64, K_INT)
            if (.not. fixnump(v)) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "integerp")
                return
            end if
        end select
        select case (trim(FWD_NAMES(k)))
        case ("fill-column")
            fill_column = xfixnum(v)
        case ("left-margin")
            left_margin = xfixnum(v)
        case ("tab-width")
            tab_width = int(xfixnum(v))
        case ("indent-tabs-mode")
            indent_tabs_mode = .not. nilp(v)
        case ("case-fold-search")
            case_fold_search = .not. nilp(v)
        case ("fill-prefix")
            call set_string_or_nil(v, fill_prefix, status)
        case ("paragraph-start")
            call set_string_or_nil(v, paragraph_start, status)
        case ("paragraph-separate")
            call set_string_or_nil(v, paragraph_separate, status)
        case ("sentence-end")
            call set_string_or_nil(v, sentence_end, status)
        case ("sentence-end-double-space")
            sentence_end_double_space = .not. nilp(v)
        case ("adaptive-fill-mode")
            adaptive_fill_mode = .not. nilp(v)
        case ("auto-fill-inhibit-regexp")
            call set_string_or_nil(v, auto_fill_inhibit_regexp, status)
        case ("standard-indent")
            standard_indent = xfixnum(v)
        case ("auto-fill-function")
            call auto_fill_from(v, auto_fill_function, status)
        case ("indent-line-function")
            call indent_line_from(v, indent_line_function, status)
        case ("transient-mark-mode")
            transient_mark_mode = .not. nilp(v)
        case ("major-mode")
            if (cur == 0) return
            if (.not. symbolp(v)) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "symbolp")
            else if (nilp(v)) then
                buffers(cur)%major_mode = "nil"
            else
                buffers(cur)%major_mode = symbols(xsymbol(v))%name
            end if
        case ("mode-name")
            if (cur == 0) return
            if (stringp(v)) then
                buffers(cur)%mode_name = xstring(v)
            else
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "stringp")
            end if
        case ("buffer-read-only")
            if (cur > 0) buffers(cur)%read_only = .not. nilp(v)
        end select
    end subroutine fwd_set

    module procedure forwarded_get
    integer :: k
    k = resolve(id)
    found = k > 0
    if (found) then
        call fwd_get(k, cur, v)
    else
        v = nil()
    end if
    end procedure forwarded_get

    module procedure forwarded_set
    integer :: k
    status = status_type()
    k = resolve(id)
    found = k > 0
    if (found) call fwd_set(k, v, status)
    end procedure forwarded_set

    module procedure auto_local
    integer :: k
    k = resolve(id)
    auto_local = symbols(id)%local_if_set
    end procedure auto_local

    ! ------------------------------------------------------------------
    ! Current values and the swap
    ! ------------------------------------------------------------------

    !> The current value of symbol ID.
    subroutine get_current(id, v, bound)
        integer(int64), intent(in) :: id
        type(value_type), intent(out) :: v
        logical, intent(out) :: bound
        integer :: k
        k = resolve(id)
        if (k > 0) then
            call fwd_get(k, cur, v)
            bound = .true.
        else
            v = symbols(id)%val
            bound = symbols(id)%bound
        end if
    end subroutine get_current

    !> Set the current value of symbol ID (not procedure variables).
    subroutine put_current(id, v, bound)
        integer(int64), intent(in) :: id
        type(value_type), intent(in) :: v
        logical, intent(in) :: bound
        integer :: k
        type(status_type) :: st
        k = resolve(id)
        if (k > 0) then
            ! The value came from the same variable, so it is valid.
            if (bound) call fwd_set(k, v, st)
        else
            symbols(id)%val = v
            symbols(id)%bound = bound
        end if
    end subroutine put_current

    !> The index of symbol ID in buffer B's local variables, or 0.
    integer function local_index(b, id) result(k)
        integer(int64), intent(in) :: b, id
        integer :: i
        k = 0
        if (b <= 0) return
        if (.not. allocated(buffers(b)%local_ids)) return
        do i = 1, buffers(b)%nlocal
            if (buffers(b)%local_ids(i) == id) then
                k = i
                return
            end if
        end do
    end function local_index

    subroutine ensure_saved_procs(b)
        integer(int64), intent(in) :: b
        type(procs_type), allocatable :: tmp(:)
        integer(int64) :: i
        if (.not. allocated(saved_procs)) allocate (saved_procs(max(b, 16_int64)))
        if (size(saved_procs, kind=int64) < b) then
            allocate (tmp(max(b, 2*size(saved_procs, kind=int64))))
            do i = 1, size(saved_procs, kind=int64)
                tmp(i)%auto_fill => saved_procs(i)%auto_fill
                tmp(i)%indent_line => saved_procs(i)%indent_line
            end do
            call move_alloc(tmp, saved_procs)
        end if
    end subroutine ensure_saved_procs

    !> Swap out the local values of buffer OLD and swap in those of NEW.
    !> Port of data.c:swap_in_symval_forwarding, for all of a buffer's
    !> local variables at once.
    subroutine swap_locals(old, new)
        integer(int64), intent(in) :: old, new
        integer :: i
        integer(int64) :: id
        if (old > 0) then
            do i = 1, buffers(old)%nlocal
                id = buffers(old)%local_ids(i)
                select case (kind_of(id))
                case (K_AUTO_FILL)
                    saved_procs(old)%auto_fill => auto_fill_function
                    auto_fill_function => default_procs%auto_fill
                case (K_INDENT_LINE)
                    saved_procs(old)%indent_line => indent_line_function
                    indent_line_function => default_procs%indent_line
                case default
                    call get_current(id, buffers(old)%local_values(i), buffers(old)%local_bound(i))
                    call put_current(id, symbols(id)%default_val, symbols(id)%default_bound)
                end select
            end do
        end if
        if (new > 0) then
            do i = 1, buffers(new)%nlocal
                id = buffers(new)%local_ids(i)
                select case (kind_of(id))
                case (K_AUTO_FILL)
                    default_procs%auto_fill => auto_fill_function
                    auto_fill_function => saved_procs(new)%auto_fill
                case (K_INDENT_LINE)
                    default_procs%indent_line => indent_line_function
                    indent_line_function => saved_procs(new)%indent_line
                case default
                    call get_current(id, symbols(id)%default_val, symbols(id)%default_bound)
                    call put_current(id, buffers(new)%local_values(i), buffers(new)%local_bound(i))
                end select
            end do
        end if
    end subroutine swap_locals

    !> Restore the default value of symbol ID, local in the current
    !> buffer at index I, and forget the local value.
    subroutine drop_local(i)
        integer, intent(in) :: i
        integer(int64) :: id
        integer :: n
        id = buffers(cur)%local_ids(i)
        select case (kind_of(id))
        case (K_AUTO_FILL)
            auto_fill_function => default_procs%auto_fill
        case (K_INDENT_LINE)
            indent_line_function => default_procs%indent_line
        case default
            call put_current(id, symbols(id)%default_val, symbols(id)%default_bound)
        end select
        n = buffers(cur)%nlocal
        buffers(cur)%local_ids(i:n - 1) = buffers(cur)%local_ids(i + 1:n)
        buffers(cur)%local_values(i:n - 1) = buffers(cur)%local_values(i + 1:n)
        buffers(cur)%local_bound(i:n - 1) = buffers(cur)%local_bound(i + 1:n)
        buffers(cur)%nlocal = n - 1
    end subroutine drop_local

    !> Check that VARIABLE is a symbol that can be made local.
    subroutine check_localizable(variable, id, status)
        type(value_type), intent(in) :: variable
        integer(int64), intent(out) :: id
        type(status_type), intent(out) :: status
        id = 0
        call ensure_obarray()
        call check_symbol(variable, status)
        if (.not. status%ok()) return
        id = xsymbol(variable)
        if (symbols(id)%constant) call set_error(status, FM_SETTING_CONSTANT, symbols(id)%name)
    end subroutine check_localizable

    ! ------------------------------------------------------------------
    ! The Lisp-level functions
    ! ------------------------------------------------------------------

    module procedure local_in_current
    if (kind_of(id) == K_SLOT) then
        local_in_current = .true.
    else
        local_in_current = local_index(cur, id) > 0
    end if
    end procedure local_in_current

    module procedure make_local_variable
    integer(int64) :: id
    integer :: n
    type(value_type), allocatable :: values(:)
    integer(int64), allocatable :: ids(:)
    logical, allocatable :: bound(:)
    call check_localizable(variable, id, status)
    if (.not. status%ok()) return
    if (cur == 0) return
    if (local_in_current(id)) return
    swap_buffer_locals_hook => swap_locals
    n = buffers(cur)%nlocal
    if (.not. allocated(buffers(cur)%local_ids)) then
        allocate (buffers(cur)%local_ids(8), buffers(cur)%local_values(8), buffers(cur)%local_bound(8))
    else if (n == size(buffers(cur)%local_ids)) then
        allocate (ids(2*n), values(2*n), bound(2*n))
        ids(1:n) = buffers(cur)%local_ids(1:n)
        values(1:n) = buffers(cur)%local_values(1:n)
        bound(1:n) = buffers(cur)%local_bound(1:n)
        call move_alloc(ids, buffers(cur)%local_ids)
        call move_alloc(values, buffers(cur)%local_values)
        call move_alloc(bound, buffers(cur)%local_bound)
    end if
    n = n + 1
    buffers(cur)%nlocal = n
    buffers(cur)%local_ids(n) = id
    ! The local value starts out as the current value, which becomes the
    ! default; the local value lives in the variable while CUR is current.
    select case (kind_of(id))
    case (K_AUTO_FILL)
        call ensure_saved_procs(cur)
        default_procs%auto_fill => auto_fill_function
    case (K_INDENT_LINE)
        call ensure_saved_procs(cur)
        default_procs%indent_line => indent_line_function
    case default
        call get_current(id, symbols(id)%default_val, symbols(id)%default_bound)
    end select
    end procedure make_local_variable

    module procedure make_variable_buffer_local
    integer(int64) :: id
    call check_localizable(variable, id, status)
    if (.not. status%ok()) return
    symbols(id)%local_if_set = .true.
    ! A void variable gets the default value nil.
    if (kind_of(id) == 0 .and. .not. local_in_current(id)) then
        if (.not. symbols(id)%bound) then
            symbols(id)%val = nil()
            symbols(id)%bound = .true.
        end if
    end if
    end procedure make_variable_buffer_local

    module procedure kill_local_variable
    integer(int64) :: id
    integer :: i
    call check_localizable(variable, id, status)
    if (.not. status%ok()) return
    i = local_index(cur, id)
    if (i > 0) call drop_local(i)
    end procedure kill_local_variable

    module procedure kill_all_local_variables
    logical :: all
    integer :: i
    type(value_type) :: prop
    type(status_type) :: st
    if (cur == 0) return
    all = .false.
    if (present(kill_permanent)) all = kill_permanent
    buffers(cur)%major_mode = "fundamental-mode"
    buffers(cur)%mode_name = "Fundamental"
    buffers(cur)%keymap = null_handle()
    do i = buffers(cur)%nlocal, 1, -1
        if (.not. all) then
            call get(make_symbol_ref(buffers(cur)%local_ids(i)), sym("permanent-local"), prop, st)
            if (.not. nilp(prop)) cycle
        end if
        call drop_local(i)
    end do
    end procedure kill_all_local_variables

    module procedure local_variable_p
    integer(int64) :: b, id
    local_variable_p = .false.
    call ensure_obarray()
    if (.not. symbolp(variable)) return
    id = xsymbol(variable)
    b = cur
    if (present(buffer)) b = buffer
    if (kind_of(id) == K_SLOT) then
        local_variable_p = .true.
    else
        local_variable_p = local_index(b, id) > 0
    end if
    end procedure local_variable_p

    module procedure local_variable_if_set_p
    local_variable_if_set_p = local_variable_p(variable, buffer)
    if (.not. local_variable_if_set_p .and. symbolp(variable)) local_variable_if_set_p = auto_local(xsymbol(variable))
    end procedure local_variable_if_set_p

    !> The default value of symbol ID.
    subroutine get_default(id, v, bound)
        integer(int64), intent(in) :: id
        type(value_type), intent(out) :: v
        logical, intent(out) :: bound
        bound = .true.
        select case (kind_of(id))
        case (K_SLOT)
            call fwd_get(resolve(id), 0_int64, v)
        case (K_AUTO_FILL)
            if (local_in_current(id)) then
                v = auto_fill_value(default_procs%auto_fill)
            else
                v = auto_fill_value(auto_fill_function)
            end if
        case (K_INDENT_LINE)
            if (local_in_current(id)) then
                v = indent_line_value(default_procs%indent_line)
            else
                v = indent_line_value(indent_line_function)
            end if
        case default
            if (local_in_current(id)) then
                v = symbols(id)%default_val
                bound = symbols(id)%default_bound
            else
                call get_current(id, v, bound)
            end if
        end select
    end subroutine get_default

    module procedure default_value
    integer(int64) :: id
    logical :: bound
    call ensure_obarray()
    call check_symbol(symbol, status)
    if (.not. status%ok()) return
    id = xsymbol(symbol)
    call get_default(id, v, bound)
    if (.not. bound) call set_error(status, FM_VOID_VARIABLE, symbols(id)%name)
    end procedure default_value

    module procedure default_boundp
    type(value_type) :: v
    default_boundp = .false.
    call ensure_obarray()
    if (.not. symbolp(symbol)) return
    call get_default(xsymbol(symbol), v, default_boundp)
    end procedure default_boundp

    module procedure set_default
    integer(int64) :: id
    integer :: k
    logical :: found
    call check_localizable(symbol, id, status)
    if (.not. status%ok()) return
    k = resolve(id)
    if (.not. local_in_current(id)) then
        call forwarded_set(id, v, status, found)
        if (found) return
        symbols(id)%val = v
        symbols(id)%bound = .true.
        return
    end if
    select case (kind_of(id))
    case (K_SLOT)
        ! The defaults of per-buffer slots are fixed.
    case (K_AUTO_FILL)
        call auto_fill_from(v, default_procs%auto_fill, status)
    case (K_INDENT_LINE)
        call indent_line_from(v, default_procs%indent_line, status)
    case (0)
        symbols(id)%default_val = v
        symbols(id)%default_bound = .true.
    case default
        ! Check the type by storing into the local value and back.
        block
            type(value_type) :: here
            logical :: bound
            call get_current(id, here, bound)
            call fwd_set(k, v, status)
            if (status%ok()) then
                symbols(id)%default_val = v
                symbols(id)%default_bound = .true.
            end if
            call put_current(id, here, bound)
        end block
    end select
    end procedure set_default

    module procedure buffer_local_value
    integer(int64) :: id
    integer :: i
    logical :: bound
    call ensure_obarray()
    call check_symbol(variable, status)
    if (.not. status%ok()) return
    id = xsymbol(variable)
    if (buffer <= 0 .or. buffer > size(buffers, kind=int64)) then
        call set_error(status, FM_WRONG_TYPE_ARGUMENT, "bufferp")
        return
    end if
    if (.not. buffer_live_p(buffer_handle(buffer))) then
        call set_error(status, FM_WRONG_TYPE_ARGUMENT, "bufferp")
        return
    end if
    bound = .true.
    if (buffer == cur) then
        call get_current(id, v, bound)
    else if (kind_of(id) == K_SLOT) then
        call fwd_get(resolve(id), buffer, v)
    else
        i = local_index(buffer, id)
        if (i == 0) then
            call get_default(id, v, bound)
        else
            select case (kind_of(id))
            case (K_AUTO_FILL)
                v = auto_fill_value(saved_procs(buffer)%auto_fill)
            case (K_INDENT_LINE)
                v = indent_line_value(saved_procs(buffer)%indent_line)
            case default
                v = buffers(buffer)%local_values(i)
                bound = buffers(buffer)%local_bound(i)
            end select
        end if
    end if
    if (.not. bound) call set_error(status, FM_VOID_VARIABLE, symbols(id)%name)
    end procedure buffer_local_value

    module procedure buffer_local_variables
    integer(int64) :: b, id
    integer :: i, n
    type(value_type), allocatable :: elts(:)
    type(value_type) :: v
    type(status_type) :: st
    character(len=*), parameter :: SLOTS(3) = [character(len=16) :: "buffer-read-only", "major-mode", "mode-name"]
    b = cur
    if (present(buffer)) b = buffer
    alist = nil()
    if (b <= 0) return
    allocate (elts(buffers(b)%nlocal + size(SLOTS)))
    n = 0
    do i = 1, buffers(b)%nlocal
        id = buffers(b)%local_ids(i)
        call buffer_local_value(make_symbol_ref(id), b, v, st)
        n = n + 1
        if (st%ok()) then
            elts(n) = list(make_symbol_ref(id), v)
        else
            elts(n) = make_symbol_ref(id)
        end if
    end do
    do i = 1, size(SLOTS)
        call buffer_local_value(sym(trim(SLOTS(i))), b, v, st)
        n = n + 1
        elts(n) = list(sym(trim(SLOTS(i))), v)
    end do
    alist = make_list(elts(1:n))
    end procedure buffer_local_variables

    module procedure setq_local
    type(value_type) :: s
    s = sym(variable)
    call make_local_variable(s, status)
    if (status%ok()) call set(s, v, status)
    end procedure setq_local
end submodule data_buffer_fm
