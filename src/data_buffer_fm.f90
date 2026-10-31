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
!> buffer.h:local_var_alist) and the variables forwarded to Fortran
!> settings (data.c's SYMBOL_FORWARDED).
!>
!> The value of a variable is always the current buffer's.  When a
!> buffer has its own value for a variable, the default is kept in the
!> symbol (`default_val') while that buffer is current, and the local
!> value in the buffer (`local_values') while it is not; the swap
!> happens in set_buffer_internal.  Variables forwarded to procedure
!> pointers (auto-fill-function, indent-line-function) keep the pointers
!> themselves in this submodule, so a procedure without a name survives
!> a swap; as values they are known by name.
!>
!> A submodule because the Fortran settings live in modules that use
!> data_fm.
submodule(data_fm) data_buffer_fm
    use lisp_fm, only: make_symbol_ref, make_list, xfixnum, xstring, value_type, vtag, V_NULL, V_INTEGER, V_NAME, &
                       V_STRING, V_ARRAY, list_elements
    use lread_fm, only: intern
    use handle_fm, only: null_handle
    use buffer_fm, only: buffers, cur, swap_buffer_locals_hook, fill_column, FILL_COLUMN_NONE, left_margin, &
                         auto_fill_function, auto_fill_proc, is_buffer_live, buffer_handle, transient_mark_mode
    use xdisp_fm, only: tab_width, TAB_WIDTH_DEFAULT
    use indent_fm, only: indent_tabs_mode, standard_indent, indent_line_function, indent_line_proc, &
                         indent_line_setting, indent_line_function_name, indent_line_function_named
    use callint_fm, only: find_command, command_name_of
    use lisp_fm, only: command_proc, value_type
    use search_fm, only: case_fold_search
    use minibuf_fm, only: completion_ignore_case
    use paragraphs_fm, only: paragraph_start, paragraph_separate, sentence_end, &
                             sentence_end_double_space, init_paragraph_variables
    use fill_fm, only: adaptive_fill_mode
    use simple_fm, only: fill_prefix, auto_fill_inhibit_regexp, do_auto_fill
    use fns_fm, only: get
    use eval_fm, only: change_major_mode_hook, kill_local_hooks
    use subr_fm, only: kill_delayed_mode_hooks
    use easy_mmode_fm, only: minor_mode_type, minor_mode_index, minor_mode_at
    use text_mode_fm, only: text_mode_variant
    use syntax_fm, only: parse_expression_ignore_comments, comment_end_can_be_escaped
    use casefiddle_fm, only: case_identifiers_as_words
    use insdel_fm, only: inhibit_modification_hooks
    use electric_fm, only: electric_indent_inhibit
    use files_fm, only: require_final_newline, mode_require_final_newline, FINAL_NEWLINE_NEVER, &
                        FINAL_NEWLINE_SAVE, FINAL_NEWLINE_VISIT, FINAL_NEWLINE_VISIT_SAVE, FINAL_NEWLINE_ASK
    implicit none(type, external)

    !> Kinds of forwarded variable.
    integer, parameter :: K_INT64 = 1, K_INT = 2, K_BOOL = 3, K_STRING = 4, K_AUTO_FILL = 5, &
                          K_INDENT_LINE = 6, K_SLOT = 7, K_FUNCTION = 8

    !> The variables forwarded to Fortran settings; slot variables are
    !> per-buffer fields of buffer_type (always local, like buffer.c's
    !> slots with a local flag of -1).
    integer, parameter :: NFWD = 28
    !> Settings forwarded with `forward_setting' come after this index
    !> (EXT_BASE + their index).
    integer, parameter :: EXT_BASE = 100000
    character(len=*), parameter :: FWD_NAMES(NFWD) = [character(len=32) :: &
                                                "fill_column", "left_margin", "tab_width", "indent_tabs_mode", "case_fold_search", &
                                                      "fill_prefix", "paragraph_start", "paragraph_separate", "sentence_end", &
                                                    "sentence_end_double_space", "adaptive_fill_mode", "auto_fill_inhibit_regexp", &
                                           "standard_indent", "auto_fill_function", "indent_line_function", "transient_mark_mode", &
                                                      "major_mode", "mode_name", "buffer_read_only", "text_mode_variant", &
                                                      "parse_expression_ignore_comments", "require_final_newline", &
                                                      "mode_require_final_newline", "case_identifiers_as_words", &
                                                      "completion_ignore_case", "comment_end_can_be_escaped", &
                                                      "inhibit_modification_hooks", &
                                                      "electric_indent_inhibit"]
    integer, parameter :: FWD_KIND(NFWD) = [K_INT64, K_INT64, K_INT, K_BOOL, K_BOOL, &
                                            K_STRING, K_STRING, K_STRING, K_STRING, &
                                            K_BOOL, K_BOOL, K_STRING, &
                                            K_INT64, K_AUTO_FILL, K_INDENT_LINE, K_BOOL, &
                                            K_SLOT, K_SLOT, K_SLOT, K_BOOL, K_BOOL, K_INT, K_INT, K_BOOL, K_BOOL, K_BOOL, K_BOOL, &
                                            K_BOOL]
    !> DEFVAR_PER_BUFFER, `make_variable_buffer_local' or `:local t':
    !> local when set.
    logical, parameter :: FWD_PER_BUFFER(NFWD) = [.true., .true., .true., .false., .true., &
                                                  .true., .false., .false., .false., &
                                                  .false., .false., .false., &
                                                  .false., .true., .false., .false., &
                                                  .true., .true., .true., .false., .false., .false., .false., .true., .false., &
                                                  .true., .false., .true.]

    !> Saved procedure pointers: the default while the current buffer
    !> has a local value, or a buffer's local value while it is not
    !> current.
    type :: procs_type
        type(function_setting) :: auto_fill
        type(indent_line_setting) :: indent_line
    end type procs_type

    type(procs_type) :: default_procs
    !> Indexed by buffer slot.
    type(procs_type), allocatable :: saved_procs(:)

    !> A saved value of a function setting forwarded with
    !> `forward_setting' (K): buffer B's local value while B is not
    !> current, or (B 0) the default while the current buffer has a local
    !> value.
    type :: fn_store_type
        integer(int64) :: b = -1
        integer :: k = 0
        type(function_setting) :: s
    end type fn_store_type

    type(fn_store_type), allocatable :: fn_store(:)

contains

    ! ------------------------------------------------------------------
    ! Forwarding
    ! ------------------------------------------------------------------

    !> The forwarding table index of symbol ID, or -1.  Not a port.
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

            if (symbols(id)%fwd < 0) call resolve_forwarded(id)
            if (symbols(id)%fwd < 0) call resolve_minor_mode(id)
        end if

        k = symbols(id)%fwd
    end function resolve

    !> Forward symbol ID to the setting forwarded with `forward_setting'
    !> under its name, if any.  Not a port.
    subroutine resolve_forwarded(id)
        integer(int64), intent(in) :: id
        integer :: i

        if (.not. allocated(forwarded)) return

        do i = 1, size(forwarded)
            if (symbols(id)%name == trim(forwarded(i)%p%name)) then
                symbols(id)%fwd = EXT_BASE + i
                if (forwarded(i)%local_if_set) symbols(id)%local_if_set = .true.
                return
            end if
        end do
    end subroutine resolve_forwarded

    !> Forward symbol ID to the state of the minor mode of that name, if
    !> the mode keeps its own state: its variable is buffer-local when
    !> set unless the mode is global.  Forwarded minor modes come after
    !> the table (NFWD + the mode's index).  Not a port.
    subroutine resolve_minor_mode(id)
        integer(int64), intent(in) :: id
        type(minor_mode_type), pointer :: m
        integer :: i

        i = minor_mode_index(symbols(id)%name)
        if (i == 0) return
        m => minor_mode_at(i)
        if (len_trim(m%variable) > 0) return
        ! A mode whose state is an expression has no variable.
        if (associated(m%getter)) return
        symbols(id)%fwd = NFWD + i
        if (.not. m%global) symbols(id)%local_if_set = .true.
    end subroutine resolve_minor_mode

    !> The kind of symbol ID: 0 for a plain variable.  Not a port.
    integer function kind_of(id) result(kind)
        integer(int64), intent(in) :: id
        integer :: k

        kind = 0
        k = resolve(id)

        if (k >= EXT_BASE) then
            select type (p => forwarded(k - EXT_BASE)%p)
            type is (integer_setting)
                kind = K_INT64
            type is (string_setting)
                kind = K_STRING
            type is (function_setting)
                kind = K_FUNCTION
            class default
                kind = K_BOOL
            end select
        else if (k > NFWD) then
            kind = K_BOOL
        else if (k > 0) then
            kind = FWD_KIND(k)
        end if
    end function kind_of

    !> The value of an auto-fill-function as a value: its name (nil for
    !> none; t for a procedure without a name).  Not a port.
    function auto_fill_value(p) result(v)
        procedure(command_proc), pointer, intent(in) :: p
        type(value_type) :: v
        character(len=:), allocatable :: name

        if (.not. associated(p)) then
            v = value_type()
            return
        end if

        name = command_name_of(p)

        if (len(name) > 0) then
            v = value_type(name=name)
        else
            v = value_type(.true.)
        end if
    end function auto_fill_value

    !> The value of an indent-line-function: its name (t for a procedure
    !> without a name).  Not a port.
    function indent_line_value(p) result(v)
        procedure(indent_line_proc), pointer, intent(in) :: p
        type(value_type) :: v
        character(len=:), allocatable :: name

        name = indent_line_function_name(p)

        if (len(name) > 0) then
            v = value_type(name=name)
        else
            v = value_type(.true.)
        end if
    end function indent_line_value

    !> N as a value, unallocated (upstream nil) when it is NONE.
    !> Not a port.
    function fixnum_or_none(n, none) result(v)
        integer(int64), intent(in) :: n, none
        type(value_type) :: v

        if (n /= none) v = value_type(n)
    end function fixnum_or_none

    ! Not a port.
    function bool_value(b) result(v)
        logical, intent(in) :: b
        type(value_type) :: v

        if (b) then
            v = value_type(.true.)
        else
            v = value_type()
        end if
    end function bool_value

    !> The value of setting S given its Fortran value as FORTRAN (for a
    !> function setting set by name, that name); unbound if void.
    !> Not a port.
    subroutine setting_value(s, fortran, v, bound)
        class(setting_type), intent(in) :: s
        type(value_type), intent(in) :: fortran
        type(value_type), intent(out) :: v
        logical, intent(out) :: bound

        bound = .not. s%void
        v = fortran

        select type (s)
        type is (function_setting)
            if (associated(s%command)) v = value_type(name=s%command%name)
        type is (indent_line_setting)
            if (associated(s%command)) v = value_type(name=s%command%name)
        end select
    end subroutine setting_value

    !> Set integer setting S to V, which must be an integer (a
    !> `wrong-type-argument' error condition occurs otherwise).  INTEGERP
    !> (a DEFVAR_PER_BUFFER with predicate `integerp') no longer changes
    !> that.  Not a port.
    subroutine set_integer(s, v, integerp, status, none)
        type(integer_setting), intent(inout) :: s
        type(value_type), intent(in) :: v
        logical, intent(in) :: integerp
        type(status_type), intent(out) :: status
        !> The value that stands for an unallocated V (upstream nil), for
        !> a setting whose nil means something (fill-column, tab-width).
        integer(int64), intent(in), optional :: none

        status = status_type()

        if (vtag(v) == V_INTEGER) then
            s = xfixnum(v)
        else if (.not. allocated(v%a) .and. present(none)) then
            s = none
        else
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer")
        end if
    end subroutine set_integer

    !> Set string setting S to V, a string or unallocated (no string); a
    !> `wrong-type-argument' error condition occurs for another value.
    !> Not a port.
    subroutine set_string(s, v, status)
        type(string_setting), intent(inout) :: s
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status

        status = status_type()

        if (vtag(v) == V_STRING) then
            s = xstring(v)
        else if (.not. allocated(v%a)) then
            call s%clear()
        else
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected character(len=*)")
        end if
    end subroutine set_string

    !> The entries of history H as a list of strings.  Not a port.
    function history_value(h) result(v)
        type(history_setting), intent(in) :: h
        type(value_type) :: v
        type(value_type), allocatable :: elems(:)
        integer :: k

        if (.not. allocated(h%items)) return
        allocate (elems(size(h%items)))

        do k = 1, size(h%items)
            elems(k) = value_type(text=h%items(k)%text)
        end do

        v = make_list(elems)
    end function history_value

    !> Set history H to V, a list of strings (a `wrong-type-argument'
    !> error condition occurs otherwise).  Not a port.
    subroutine set_history(h, v, status)
        type(history_setting), intent(inout) :: h
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: elems(:)
        type(history_item_type), allocatable :: items(:)
        integer :: k

        status = status_type()

        if (allocated(v%a)) then
            if (vtag(v) /= V_ARRAY) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected array of character(len=*)")
                return
            end if
        end if

        call list_elements(v, elems)
        allocate (items(size(elems)))

        do k = 1, size(elems)
            if (vtag(elems(k)) /= V_STRING) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected array of character(len=*)")
                return
            end if

            items(k)%text = xstring(elems(k))
        end do

        call move_alloc(items, h%items)
        h%void = .false.
    end subroutine set_history

    !> The value of forwarded variable K in buffer B (B matters only for
    !> slot variables; the others read the current value); BOUND is false
    !> when it is void.  Not a port.
    subroutine fwd_get(k, b, v, bound)
        integer, intent(in) :: k
        integer(int64), intent(in) :: b
        type(value_type), intent(out) :: v
        logical, intent(out) :: bound
        type(minor_mode_type), pointer :: m

        bound = .true.

        if (k >= EXT_BASE) then
            select type (p => forwarded(k - EXT_BASE)%p)
            type is (integer_setting)
                call setting_value(p, value_type(p%value), v, bound)
            type is (logical_setting)
                call setting_value(p, bool_value(p%value), v, bound)
            type is (string_setting)
                call setting_value(p, string_or_nil(p%value), v, bound)
            type is (function_setting)
                call setting_value(p, auto_fill_value(p%value), v, bound)
            type is (history_setting)
                call setting_value(p, history_value(p), v, bound)
            class default
                v = value_type()
            end select

            return
        end if

        if (k > NFWD) then
            m => minor_mode_at(k - NFWD)
            call setting_value(m%state, bool_value(m%state%value), v, bound)
            return
        end if

        select case (trim(FWD_NAMES(k)))
        case ("fill_column")
            call setting_value(fill_column, fixnum_or_none(fill_column%value, FILL_COLUMN_NONE), v, bound)
        case ("left_margin")
            call setting_value(left_margin, value_type(left_margin%value), v, bound)
        case ("tab_width")
            call setting_value(tab_width, fixnum_or_none(tab_width%value, TAB_WIDTH_DEFAULT), v, bound)
        case ("indent_tabs_mode")
            ! A DEFVAR_BOOL: always t or nil.
            v = bool_value(indent_tabs_mode%value)
        case ("completion_ignore_case")
            v = bool_value(completion_ignore_case%value)
        case ("case_fold_search")
            call setting_value(case_fold_search, bool_value(case_fold_search%value), v, bound)
        case ("fill_prefix")
            call setting_value(fill_prefix, string_or_nil(fill_prefix%value), v, bound)
        case ("paragraph_start")
            call init_paragraph_variables()
            call setting_value(paragraph_start, string_or_nil(paragraph_start%value), v, bound)
        case ("paragraph_separate")
            call init_paragraph_variables()
            call setting_value(paragraph_separate, string_or_nil(paragraph_separate%value), v, bound)
        case ("sentence_end")
            call setting_value(sentence_end, string_or_nil(sentence_end%value), v, bound)
        case ("sentence_end_double_space")
            call setting_value(sentence_end_double_space, bool_value(sentence_end_double_space%value), v, bound)
        case ("adaptive_fill_mode")
            call setting_value(adaptive_fill_mode, bool_value(adaptive_fill_mode%value), v, bound)
        case ("auto_fill_inhibit_regexp")
            call setting_value(auto_fill_inhibit_regexp, string_or_nil(auto_fill_inhibit_regexp%value), v, bound)
        case ("standard_indent")
            call setting_value(standard_indent, value_type(standard_indent%value), v, bound)
        case ("auto_fill_function")
            call setting_value(auto_fill_function, auto_fill_value(auto_fill_function%value), v, bound)
        case ("indent_line_function")
            call setting_value(indent_line_function, indent_line_value(indent_line_function%value), v, bound)
        case ("transient_mark_mode")
            call setting_value(transient_mark_mode, bool_value(transient_mark_mode%value), v, bound)
        case ("electric_indent_inhibit")
            call setting_value(electric_indent_inhibit, bool_value(electric_indent_inhibit%value), v, bound)
        case ("major_mode")
            v = value_type(name="fundamental_mode")

            if (b > 0) then
                if (allocated(buffers(b)%major_mode)) v = value_type(name=buffers(b)%major_mode)
            end if
        case ("mode_name")
            v = value_type(text="Fundamental")

            if (b > 0) then
                if (allocated(buffers(b)%mode_name)) v = value_type(text=buffers(b)%mode_name)
            end if
        case ("buffer_read_only")
            v = value_type()
            if (b > 0) v = bool_value(buffers(b)%read_only)
        case ("text_mode_variant")
            call setting_value(text_mode_variant, bool_value(text_mode_variant%value), v, bound)
        case ("parse_expression_ignore_comments")
            ! A DEFVAR_BOOL: always t or nil.
            v = bool_value(parse_expression_ignore_comments%value)
        case ("case_identifiers_as_words")
            ! A DEFVAR_BOOL: always t or nil.
            v = bool_value(case_identifiers_as_words%value)
        case ("comment_end_can_be_escaped")
            ! A DEFVAR_BOOL: always t or nil.
            v = bool_value(comment_end_can_be_escaped%value)
        case ("inhibit_modification_hooks")
            ! A DEFVAR_BOOL: always t or nil.
            v = bool_value(inhibit_modification_hooks%value)
        case ("require_final_newline")
            call setting_value(require_final_newline, final_newline_value(require_final_newline%value), v, bound)
        case ("mode_require_final_newline")
            call setting_value(mode_require_final_newline, final_newline_value(mode_require_final_newline%value), &
                               v, bound)
        case default
            v = value_type()
        end select
    end subroutine fwd_get

    !> A string or nil.  Not a port.
    function string_or_nil(s) result(v)
        character(len=:), allocatable, intent(in) :: s
        type(value_type) :: v

        if (allocated(s)) then
            v = value_type(text=s)
        else
            v = value_type()
        end if
    end function string_or_nil

    !> Set forwarded variable K (of the current buffer, for slots).
    !> Port of data.c:store_symval_forwarding.
    subroutine fwd_set(k, v, status)
        integer, intent(in) :: k
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status
        type(minor_mode_type), pointer :: m

        status = status_type()

        if (k >= EXT_BASE) then
            select type (p => forwarded(k - EXT_BASE)%p)
            type is (integer_setting)
                call set_integer(p, v, .false., status)
            type is (logical_setting)
                p = allocated(v%a)
            type is (string_setting)
                call set_string(p, v, status)
            type is (function_setting)
                call set_auto_fill(p, v, status)
            type is (history_setting)
                call set_history(p, v, status)
            class default
            end select

            return
        end if

        if (k > NFWD) then
            m => minor_mode_at(k - NFWD)
            m%state = allocated(v%a)
            return
        end if

        select case (trim(FWD_NAMES(k)))
        case ("fill_column")
            call set_integer(fill_column, v, .true., status, FILL_COLUMN_NONE)
        case ("left_margin")
            call set_integer(left_margin, v, .true., status)
        case ("tab_width")
            call set_integer(tab_width, v, .true., status, TAB_WIDTH_DEFAULT)
        case ("indent_tabs_mode")
            indent_tabs_mode = allocated(v%a)
        case ("completion_ignore_case")
            completion_ignore_case = allocated(v%a)
        case ("case_fold_search")
            case_fold_search = allocated(v%a)
        case ("fill_prefix")
            call set_string(fill_prefix, v, status)
        case ("paragraph_start")
            call set_string(paragraph_start, v, status)
        case ("paragraph_separate")
            call set_string(paragraph_separate, v, status)
        case ("sentence_end")
            call set_string(sentence_end, v, status)
        case ("sentence_end_double_space")
            sentence_end_double_space = allocated(v%a)
        case ("adaptive_fill_mode")
            adaptive_fill_mode = allocated(v%a)
        case ("auto_fill_inhibit_regexp")
            call set_string(auto_fill_inhibit_regexp, v, status)
        case ("standard_indent")
            call set_integer(standard_indent, v, .false., status)
        case ("auto_fill_function")
            call set_auto_fill(auto_fill_function, v, status)
        case ("indent_line_function")
            call set_indent_line(indent_line_function, v, status)
        case ("transient_mark_mode")
            transient_mark_mode = allocated(v%a)
        case ("electric_indent_inhibit")
            electric_indent_inhibit = allocated(v%a)
        case ("major_mode")
            if (cur == 0) return

            if ((vtag(v) /= V_NAME .and. vtag(v) /= V_NULL)) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected name")
            else if (.not. allocated(v%a)) then
                buffers(cur)%major_mode = "nil"
            else
                buffers(cur)%major_mode = symbols(xsymbol(v))%name
            end if
        case ("mode_name")
            if (cur == 0) return

            if (vtag(v) == V_STRING) then
                buffers(cur)%mode_name = xstring(v)
            else
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected character(len=*)")
            end if
        case ("buffer_read_only")
            if (cur > 0) buffers(cur)%read_only = allocated(v%a)
        case ("text_mode_variant")
            text_mode_variant = allocated(v%a)
        case ("parse_expression_ignore_comments")
            parse_expression_ignore_comments = allocated(v%a)
        case ("case_identifiers_as_words")
            case_identifiers_as_words = allocated(v%a)
        case ("comment_end_can_be_escaped")
            comment_end_can_be_escaped = allocated(v%a)
        case ("inhibit_modification_hooks")
            inhibit_modification_hooks = allocated(v%a)
        case ("require_final_newline")
            call set_final_newline(require_final_newline, v)
        case ("mode_require_final_newline")
            call set_final_newline(mode_require_final_newline, v)
        case default
            ! Not a forwarded variable (resolve gives no such K).
        end select
    end subroutine fwd_set

    !> auto-fill-function := V: the procedure of the function V names,
    !> resolved now; a `void-function' error condition occurs for a name
    !> that is not defined, and `wrong-type-argument' for another value.
    !> Not a port.
    subroutine set_auto_fill(s, v, status)
        type(function_setting), intent(inout) :: s
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status
        type(command_type), pointer :: c

        status = status_type()

        if (.not. allocated(v%a)) then
            s%value => null()
            s%command => null()
            call settle(s)
            return
        end if

        if (vtag(v) /= V_NAME) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected name")
            return
        end if

        c => find_command(symbols(xsymbol(v))%name)

        if (.not. associated(c)) then
            call set_error(status, FM_VOID_FUNCTION, symbols(xsymbol(v))%name)
            return
        end if

        s%value => c%execute
        ! The name the value reads back as.
        s%command => c
        call settle(s)
    end subroutine set_auto_fill

    !> indent-line-function := V, the name of an indent-line function, or
    !> of a command (called by `indent_line_by_name'), resolved now; a
    !> `void-function' error condition occurs for a name that is not
    !> defined, and `wrong-type-argument' for another value.  Not a port.
    subroutine set_indent_line(s, v, status)
        type(indent_line_setting), intent(inout) :: s
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status
        type(command_type), pointer :: c

        status = status_type()

        if (vtag(v) /= V_NAME) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected name")
            return
        end if

        call indent_line_function_named(symbols(xsymbol(v))%name, s%value)
        s%command => null()

        if (len(indent_line_function_name(s%value)) == 0) then
            c => find_command(symbols(xsymbol(v))%name)

            if (.not. associated(c)) then
                call set_error(status, FM_VOID_FUNCTION, symbols(xsymbol(v))%name)
                return
            end if

            s%command => c
        end if

        call settle(s)
    end subroutine set_indent_line

    !> The value of a require-final-newline setting N: nil, t, `visit',
    !> `visit_save' or (for any other value) `ask'.  Not a port.
    function final_newline_value(n) result(v)
        integer(int64), intent(in) :: n
        type(value_type) :: v

        select case (n)
        case (FINAL_NEWLINE_NEVER)
            v = value_type()
        case (FINAL_NEWLINE_SAVE)
            v = value_type(.true.)
        case (FINAL_NEWLINE_VISIT)
            v = value_type(name="visit")
        case (FINAL_NEWLINE_VISIT_SAVE)
            v = value_type(name="visit_save")
        case default
            v = value_type(name="ask")
        end select
    end function final_newline_value

    !> Set require-final-newline setting S to V.  Not a port.
    subroutine set_final_newline(s, v)
        type(integer_setting), intent(inout) :: s
        type(value_type), intent(in) :: v

        if (.not. allocated(v%a)) then
            s = FINAL_NEWLINE_NEVER
        else if (value_eq(v, value_type(.true.))) then
            s = FINAL_NEWLINE_SAVE
        else if (value_eq(v, value_type(name="visit"))) then
            s = FINAL_NEWLINE_VISIT
        else if (value_eq(v, value_type(name="visit_save"))) then
            s = FINAL_NEWLINE_VISIT_SAVE
        else
            s = FINAL_NEWLINE_ASK
        end if
    end subroutine set_final_newline

    !> Forget S's voidness.  Not a port.
    subroutine settle(s)
        class(setting_type), intent(inout) :: s

        s%void = .false.
    end subroutine settle

    !> makunbound on forwarded variable K: Fset with Qunbound, which
    !> store_symval_forwarding checks against the variable's kind.
    !> Not a port.
    subroutine fwd_makunbound(k, status)
        integer, intent(in) :: k
        type(status_type), intent(out) :: status
        type(minor_mode_type), pointer :: m

        status = status_type()

        if (k >= EXT_BASE) then
            forwarded(k - EXT_BASE)%p%void = .true.
            return
        end if

        if (k > NFWD) then
            m => minor_mode_at(k - NFWD)
            m%state%void = .true.
            return
        end if

        select case (trim(FWD_NAMES(k)))
        case ("fill_column", "left_margin", "tab_width")
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer")
        case ("indent_tabs_mode")
            ! A DEFVAR_BOOL stores !NILP (Qunbound): t.
            indent_tabs_mode = .true.
        case ("completion_ignore_case")
            completion_ignore_case = .true.
        case ("major_mode")
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected name")
        case ("mode_name", "buffer_read_only")
            continue
        case ("case_fold_search")
            case_fold_search%void = .true.
        case ("fill_prefix")
            fill_prefix%void = .true.
        case ("paragraph_start")
            paragraph_start%void = .true.
        case ("paragraph_separate")
            paragraph_separate%void = .true.
        case ("sentence_end")
            sentence_end%void = .true.
        case ("sentence_end_double_space")
            sentence_end_double_space%void = .true.
        case ("adaptive_fill_mode")
            adaptive_fill_mode%void = .true.
        case ("auto_fill_inhibit_regexp")
            auto_fill_inhibit_regexp%void = .true.
        case ("standard_indent")
            standard_indent%void = .true.
        case ("auto_fill_function")
            auto_fill_function%void = .true.
        case ("indent_line_function")
            indent_line_function%void = .true.
        case ("transient_mark_mode")
            transient_mark_mode%void = .true.
        case ("electric_indent_inhibit")
            electric_indent_inhibit%void = .true.
        case ("text_mode_variant")
            text_mode_variant%void = .true.
        case ("parse_expression_ignore_comments")
            ! A DEFVAR_BOOL stores !NILP (Qunbound): t.
            parse_expression_ignore_comments = .true.
        case ("case_identifiers_as_words")
            ! A DEFVAR_BOOL stores !NILP (Qunbound): t.
            case_identifiers_as_words = .true.
        case ("comment_end_can_be_escaped")
            ! A DEFVAR_BOOL stores !NILP (Qunbound): t.
            comment_end_can_be_escaped = .true.
        case ("inhibit_modification_hooks")
            ! A DEFVAR_BOOL stores !NILP (Qunbound): t.
            inhibit_modification_hooks = .true.
        case ("require_final_newline")
            require_final_newline%void = .true.
        case ("mode_require_final_newline")
            mode_require_final_newline%void = .true.
        case default
            continue
        end select
    end subroutine fwd_makunbound

    module procedure forwarded_get
    integer :: k
    logical :: is_bound

    k = resolve(id)
    found = k > 0
    is_bound = .false.

    if (found) then
        call fwd_get(k, cur, v, is_bound)
    else
        v = value_type()
    end if

    if (present(bound)) bound = is_bound
    end procedure forwarded_get

    module procedure forwarded_makunbound
    integer :: k

    status = status_type()
    k = resolve(id)
    found = k > 0
    if (found) call fwd_makunbound(k, status)
    end procedure forwarded_makunbound

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

    !> The current value of symbol ID.  Not a port.
    subroutine get_current(id, v, bound)
        integer(int64), intent(in) :: id
        type(value_type), intent(out) :: v
        logical, intent(out) :: bound
        integer :: k

        k = resolve(id)

        if (k > 0) then
            call fwd_get(k, cur, v, bound)
        else
            v = symbols(id)%val
            bound = symbols(id)%bound
        end if
    end subroutine get_current

    !> Set the current value of symbol ID (not procedure variables).
    !> Not a port.
    subroutine set_current(id, v, bound)
        integer(int64), intent(in) :: id
        type(value_type), intent(in) :: v
        logical, intent(in) :: bound
        integer :: k
        type(status_type) :: st

        k = resolve(id)

        if (k > 0) then
            ! The value came from the same variable, so it is valid.
            if (bound) then
                call fwd_set(k, v, st)
            else
                call fwd_makunbound(k, st)
            end if
        else
            symbols(id)%val = v
            symbols(id)%bound = bound
        end if
    end subroutine set_current

    !> The index of symbol ID in buffer B's local variables, or 0.
    !> Not a port.
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

    ! Not a port.
    subroutine ensure_saved_procs(b)
        integer(int64), intent(in) :: b
        type(procs_type), allocatable :: tmp(:)
        integer(int64) :: i

        if (.not. allocated(saved_procs)) allocate (saved_procs(max(b, 16_int64)))

        if (size(saved_procs, kind=int64) < b) then
            allocate (tmp(max(b, 2*size(saved_procs, kind=int64))))

            do i = 1, size(saved_procs, kind=int64)
                tmp(i) = saved_procs(i)
            end do

            call move_alloc(tmp, saved_procs)
        end if
    end subroutine ensure_saved_procs

    !> The index in `fn_store' of (B, K), made if MAKE; 0 if none.
    !> Not a port.
    integer function fn_index(b, k, make) result(i)
        integer(int64), intent(in) :: b
        integer, intent(in) :: k
        logical, intent(in) :: make
        type(fn_store_type), allocatable :: grown(:)
        integer :: j, n

        i = 0
        if (.not. allocated(fn_store)) allocate (fn_store(0))

        do j = 1, size(fn_store)
            if (fn_store(j)%b == b .and. fn_store(j)%k == k) then
                i = j
                return
            end if
        end do

        if (.not. make) return
        n = size(fn_store)
        allocate (grown(n + 1))

        do j = 1, n
            grown(j) = fn_store(j)
        end do

        grown(n + 1)%b = b
        grown(n + 1)%k = k
        call move_alloc(grown, fn_store)
        i = n + 1
    end function fn_index

    !> Save the current value of the forwarded function setting of symbol
    !> ID as (B, its index).  Not a port.
    subroutine fn_begin(id, b)
        integer(int64), intent(in) :: id, b
        integer :: k, i

        k = resolve(id)
        i = fn_index(b, k, .true.)

        select type (p => forwarded(k - EXT_BASE)%p)
        type is (function_setting)
            fn_store(i)%s = p
        end select
    end subroutine fn_begin

    !> Make the value saved as (B, the index of symbol ID) current, if any.
    !> Not a port.
    subroutine fn_end(id, b)
        integer(int64), intent(in) :: id, b
        integer :: k, i

        k = resolve(id)
        i = fn_index(b, k, .false.)
        if (i == 0) return

        select type (p => forwarded(k - EXT_BASE)%p)
        type is (function_setting)
            p = fn_store(i)%s
        end select
    end subroutine fn_end

    !> The value saved as (B, the index of symbol ID).  Not a port.
    subroutine fn_saved_value(id, b, v, bound)
        integer(int64), intent(in) :: id, b
        type(value_type), intent(out) :: v
        logical, intent(out) :: bound
        integer :: i

        v = value_type()
        bound = .true.
        i = fn_index(b, resolve(id), .false.)
        if (i == 0) return
        call setting_value(fn_store(i)%s, auto_fill_value(fn_store(i)%s%value), v, bound)
    end subroutine fn_saved_value

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
                    saved_procs(old)%auto_fill = auto_fill_function
                    auto_fill_function = default_procs%auto_fill
                case (K_INDENT_LINE)
                    saved_procs(old)%indent_line = indent_line_function
                    indent_line_function = default_procs%indent_line
                case (K_FUNCTION)
                    call fn_begin(id, old)
                    call fn_end(id, 0_int64)
                case default
                    call get_current(id, buffers(old)%local_values(i), buffers(old)%local_bound(i))
                    call set_current(id, symbols(id)%default_val, symbols(id)%default_bound)
                end select
            end do
        end if

        if (new > 0) then
            do i = 1, buffers(new)%nlocal
                id = buffers(new)%local_ids(i)

                select case (kind_of(id))
                case (K_AUTO_FILL)
                    default_procs%auto_fill = auto_fill_function
                    auto_fill_function = saved_procs(new)%auto_fill
                case (K_INDENT_LINE)
                    default_procs%indent_line = indent_line_function
                    indent_line_function = saved_procs(new)%indent_line
                case (K_FUNCTION)
                    call fn_begin(id, 0_int64)
                    call fn_end(id, new)
                case default
                    call get_current(id, symbols(id)%default_val, symbols(id)%default_bound)
                    call set_current(id, buffers(new)%local_values(i), buffers(new)%local_bound(i))
                end select
            end do
        end if
    end subroutine swap_locals

    !> Restore the default value of symbol ID, local in the current
    !> buffer at index I, and forget the local value.  Not a port.
    subroutine kill_local(i)
        integer, intent(in) :: i
        integer(int64) :: id
        integer :: n

        id = buffers(cur)%local_ids(i)

        select case (kind_of(id))
        case (K_AUTO_FILL)
            auto_fill_function = default_procs%auto_fill
        case (K_INDENT_LINE)
            indent_line_function = default_procs%indent_line
        case (K_FUNCTION)
            call fn_end(id, 0_int64)
        case default
            call set_current(id, symbols(id)%default_val, symbols(id)%default_bound)
        end select

        n = buffers(cur)%nlocal
        buffers(cur)%local_ids(i:n - 1) = buffers(cur)%local_ids(i + 1:n)
        buffers(cur)%local_values(i:n - 1) = buffers(cur)%local_values(i + 1:n)
        buffers(cur)%local_bound(i:n - 1) = buffers(cur)%local_bound(i + 1:n)
        buffers(cur)%nlocal = n - 1
    end subroutine kill_local

    !> Check that VARIABLE is a symbol that can be made local.  Not a port.
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
        default_procs%auto_fill = auto_fill_function
    case (K_INDENT_LINE)
        call ensure_saved_procs(cur)
        default_procs%indent_line = indent_line_function
    case (K_FUNCTION)
        call fn_begin(id, 0_int64)
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
            symbols(id)%val = value_type()
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
    if (i > 0) call kill_local(i)
    end procedure kill_local_variable

    module procedure kill_all_local_variables
    logical :: all
    integer :: i
    type(value_type) :: prop
    type(status_type) :: st

    if (cur == 0) return
    ! The first thing this function does is run the normal hook
    ! `change_major_mode_hook'.
    call change_major_mode_hook%execute(st)
    all = .false.
    if (present(kill_permanent)) all = kill_permanent
    call kill_local_hooks(all)
    call kill_delayed_mode_hooks()
    buffers(cur)%syntax_table = null_handle()
    buffers(cur)%downcase_table = null_handle()
    buffers(cur)%upcase_table = null_handle()
    buffers(cur)%case_canon_table = null_handle()
    buffers(cur)%case_eqv_table = null_handle()
    buffers(cur)%category_table = null_handle()
    buffers(cur)%major_mode = "fundamental_mode"
    buffers(cur)%mode_name = "Fundamental"
    buffers(cur)%keymap = null_handle()
    buffers(cur)%invisibility_spec_t = .true.
    buffers(cur)%bidi_display_reordering = .true.
    buffers(cur)%bidi_paragraph_direction = 0
    buffers(cur)%read_only_mode_state_set = .false.
    if (allocated(buffers(cur)%invisibility_spec)) deallocate (buffers(cur)%invisibility_spec)

    do i = buffers(cur)%nlocal, 1, -1
        if (.not. all) then
            call get(make_symbol_ref(buffers(cur)%local_ids(i)), value_type(name="permanent_local"), prop, st)
            if (allocated(prop%a)) cycle
        end if

        call kill_local(i)
    end do

    ! The buffer-local variables kept in Fortran objects (none of them
    ! permanent).
    if (allocated(killers)) then
        do i = 1, size(killers)
            call killers(i)%fn()
        end do
    end if
    end procedure kill_all_local_variables

    module procedure is_local_variable
    integer(int64) :: b, id

    is_local_variable = .false.
    call ensure_obarray()
    if ((vtag(variable) /= V_NAME .and. vtag(variable) /= V_NULL)) return
    id = xsymbol(variable)
    b = cur
    if (present(buffer)) b = buffer

    if (kind_of(id) == K_SLOT) then
        is_local_variable = .true.
    else
        is_local_variable = local_index(b, id) > 0
    end if
    end procedure is_local_variable

    module procedure is_automatically_local
    is_automatically_local = .false.
    if ((vtag(variable) == V_NAME .or. vtag(variable) == V_NULL)) is_automatically_local = auto_local(xsymbol(variable))
    end procedure is_automatically_local

    module procedure is_forwarded_variable
    is_forwarded_variable = .false.
    if ((vtag(variable) == V_NAME .or. vtag(variable) == V_NULL)) is_forwarded_variable = resolve(xsymbol(variable)) > 0
    end procedure is_forwarded_variable

    module procedure is_local_variable_if_set
    is_local_variable_if_set = is_local_variable(variable, buffer)

    if (.not. is_local_variable_if_set .and. (vtag(variable) == V_NAME .or. vtag(variable) == V_NULL)) then
        is_local_variable_if_set = auto_local(xsymbol(variable))
    end if
    end procedure is_local_variable_if_set

    !> The default value of symbol ID.  Not a port.
    subroutine get_default(id, v, bound)
        integer(int64), intent(in) :: id
        type(value_type), intent(out) :: v
        logical, intent(out) :: bound

        bound = .true.

        select case (kind_of(id))
        case (K_SLOT)
            call fwd_get(resolve(id), 0_int64, v, bound)
        case (K_AUTO_FILL)
            if (local_in_current(id)) then
                call setting_value(default_procs%auto_fill, auto_fill_value(default_procs%auto_fill%value), v, bound)
            else
                call get_current(id, v, bound)
            end if
        case (K_INDENT_LINE)
            if (local_in_current(id)) then
                call setting_value(default_procs%indent_line, indent_line_value(default_procs%indent_line%value), &
                                   v, bound)
            else
                call get_current(id, v, bound)
            end if
        case (K_FUNCTION)
            if (local_in_current(id)) then
                call fn_saved_value(id, 0_int64, v, bound)
            else
                call get_current(id, v, bound)
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

    module procedure function_setting_default
    integer(int64) :: id
    integer :: i

    fn => s%value
    call ensure_obarray()
    id = xsymbol(value_type(name=trim(s%name)))
    if (.not. is_local_variable(value_type(name=trim(s%name)))) return
    ! The default is saved as (0, the setting's index) while the current
    ! buffer has its own value.
    i = fn_index(0_int64, resolve(id), .false.)
    if (i > 0) fn => fn_store(i)%s%value
    end procedure function_setting_default

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
    if ((vtag(symbol) /= V_NAME .and. vtag(symbol) /= V_NULL)) return
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
        call set_auto_fill(default_procs%auto_fill, v, status)
    case (K_INDENT_LINE)
        call set_indent_line(default_procs%indent_line, v, status)
    case (K_FUNCTION)
        block
            integer :: i
            i = fn_index(0_int64, resolve(id), .true.)
            call set_auto_fill(fn_store(i)%s, v, status)
        end block
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

            call set_current(id, here, bound)
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
        call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(buffer_type)")
        return
    end if

    if (.not. is_buffer_live(buffer_handle(buffer))) then
        call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(buffer_type)")
        return
    end if

    bound = .true.

    if (buffer == cur) then
        call get_current(id, v, bound)
    else if (kind_of(id) == K_SLOT) then
        call fwd_get(resolve(id), buffer, v, bound)
    else
        i = local_index(buffer, id)

        if (i == 0) then
            call get_default(id, v, bound)
        else
            select case (kind_of(id))
            case (K_AUTO_FILL)
                call setting_value(saved_procs(buffer)%auto_fill, auto_fill_value(saved_procs(buffer)%auto_fill%value), &
                                   v, bound)
            case (K_INDENT_LINE)
                call setting_value(saved_procs(buffer)%indent_line, &
                                   indent_line_value(saved_procs(buffer)%indent_line%value), v, bound)
            case (K_FUNCTION)
                call fn_saved_value(id, buffer, v, bound)
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
    character(len=*), parameter :: SLOTS(3) = [character(len=16) :: "buffer_read_only", "major_mode", "mode_name"]

    b = cur
    if (present(buffer)) b = buffer
    table = value_type()
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
        call buffer_local_value(value_type(name=trim(SLOTS(i))), b, v, st)
        n = n + 1
        elts(n) = list(value_type(name=trim(SLOTS(i))), v)
    end do

    table = make_list(elts(1:n))
    end procedure buffer_local_variables

    module procedure setq_local
    type(value_type) :: s

    s = value_type(name=variable)
    call make_local_variable(s, status)
    if (status%ok()) call set(s, v, status)
    end procedure setq_local
end submodule data_buffer_fm
