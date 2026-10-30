! lisp_fm.f90 -- the value type and symbol storage.
! After GNU Emacs 31.1 src/lisp.h (Lisp_Object, struct Lisp_Symbol) and
! the constructors of src/alloc.c.
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

!> Values: what upstream represents as Lisp_Object.
!>
!> There is no Lisp here.  A `value_type' is a plain Fortran derived type
!> with value semantics: assignment deep-copies and allocatables free
!> themselves, so no garbage collector is needed and no cycles can form.
!> Shared mutable objects (buffers, markers, later keymaps and windows)
!> live in registries; a value only carries their handle.
!>
!> Representation.  A value is a tree stored flat, in preorder, in an
!> array of atoms.  An atom is a scalar (integer, symbol, string, handle)
!> or a list header holding its element count and its subtree size
!> (`span').  So (a (b c) d) is
!>
!>     [LIST n=3 span=6] [a] [LIST n=2 span=3] [b] [c] [d]
!>
!> A string with text properties keeps them as its children: the
!> string atom holds the number of property intervals and spans them,
!> each a list (START END PROP VALUE ...) of 0-based character
!> positions, sorted and not overlapping, with non-empty plists (the
!> interval tree upstream keeps for a string).
!>
!> A recursive type (a value holding an array of values) would be more
!> direct, but gfortran 13 and 14 copy such components shallowly, which
!> breaks value semantics and double-frees.  Code outside this module and
!> data_fm/fns_fm should use the procedures, not the atoms.
!>
!> Lists are proper lists only: no dotted pairs, no setcar/setcdr
!> sharing.  Without object identity, `eq' on strings and lists is always
!> false (data_fm:value_eq), as for distinct objects upstream.
!>
!> gfortran leaks a function result with allocatable components when it
!> appears inside an array constructor, so build lists with `list(...)'
!> or from variables: write `list(sym("a"), sym("b"))', never
!> `make_list([sym("a"), sym("b")])'.
!>
!> Symbols are stored here (struct Lisp_Symbol); interning is lread_fm,
!> values and properties are data_fm and fns_fm.  Symbols are never
!> freed, so a symbol is just an id; ids 1 and 2 are `nil' and `t'.
module lisp_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use handle_fm, only: handle_type
    use error_fm, only: status_type
    implicit none(type, external)
    private

    abstract interface
        !> A command or function: it reads the prefix argument (commands
        !> only) and reports errors through `status'.
        subroutine command_proc(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine command_proc
    end interface

    !> Values of a command property that is unset (nil), t, or `no'.
    integer, parameter, public :: PROPERTY_UNSET = 0, PROPERTY_TRUE = 1, PROPERTY_NO = 2

    !> A named function: what upstream keeps in a symbol's function cell,
    !> its interactive spec (`interactive': a command) and its `disabled'
    !> property.  Built-in ones are the components of `commands'
    !> (commands_fm); define your own as a `target' variable and call
    !> its `define':
    !>
    !>     type(command_type), target :: insert_date_stamp_command
    !>     insert_date_stamp_command = command_type("insert_date_stamp", insert_date_stamp)
    !>     call insert_date_stamp_command%define()
    type, public :: command_type
        !> The name, as M-x and the help commands show it.
        character(len=:), allocatable :: name
        !> The procedure; null if the command is not defined.
        procedure(command_proc), pointer, nopass :: run => null()
        !> Whether it is a command (has an interactive spec), so M-x and
        !> keys can run it.
        logical :: interactive = .true.
        !> True means running it interactively asks for confirmation
        !> first (novice.el; the `disabled' property).
        logical :: disabled = .false.
        !> A message to show instead of the standard one when `disabled'
        !> (a string `disabled' property).
        character(len=:), allocatable :: disabled_message
        !> A key sequence (`key_parse' syntax) that `where_is_internal'
        !> returns first for this command when it still runs it, as the
        !> one key to show in menus and docstrings (the
        !> `:advertised-binding' property; one key here).
        character(len=:), allocatable :: advertised_binding
        !> The keymap of single keys that repeat this command in Repeat
        !> mode (the `repeat-map' property; the null handle for none).
        type(handle_type) :: repeat_map
        !> Whether the command continues the current repeating sequence
        !> whatever its map (the `repeat-continue' property t), or the
        !> repeat maps whose sequences it continues (a list of keymaps).
        logical :: repeat_continue_all = .false.
        type(handle_type), allocatable :: repeat_continue(:)
        !> Whether Repeat mode checks that the last key is in the repeat
        !> map (the `repeat-check-key' property): PROPERTY_UNSET to use
        !> `repeat_check_key', PROPERTY_TRUE, or PROPERTY_NO.
        integer :: repeat_check_key = PROPERTY_UNSET
        !> The idle time after which the repeating sequence ends (the
        !> `repeat-exit-timeout' property; negative to use
        !> `repeat_exit_timeout'), or no timeout (`repeat_exit_timeout_no',
        !> the value `no').
        real :: repeat_exit_timeout = -1.0
        logical :: repeat_exit_timeout_no = .false.
        !> A hint shown by Repeat mode for the command's key (the
        !> `repeat-hint' property); unallocated for none.
        character(len=:), allocatable :: repeat_hint
    contains
        !> Make this command known by its name, to M-x, keymaps and the
        !> help commands (upstream's defun).
        procedure :: define => define_command
    end type command_type

    !> The pointer to a defined command.
    type :: command_ref_type
        type(command_type), pointer :: p => null()
    end type command_ref_type

    !> Commands defined with `define', not built in.
    type(command_ref_type), allocatable :: defined_commands(:)
    integer :: n_defined_commands = 0

    public :: command_proc, defined_command, defined_command_name_of

    !> Value tags.
    integer(int32), parameter, public :: V_NIL = 0, V_INT = 1, V_SYMBOL = 2, &
                                         V_STRING = 3, V_LIST = 4, &
                                         V_BUFFER = 5, V_MARKER = 6, V_KEYMAP = 7, &
                                         V_SYNTAX_TABLE = 8, V_WINDOW = 9

    !> Ids of the predefined symbols.
    integer(int64), parameter, public :: SYM_NIL = 1, SYM_T = 2

    !> One node of a value's tree.
    type, public :: atom_type
        integer(int32) :: tag = V_NIL
        !> V_INT: the integer; V_SYMBOL: the symbol id; V_LIST: element count;
        !> V_STRING: the number of text property intervals.
        integer(int64) :: i = 0
        !> Atoms in this subtree, itself included (1 for scalars).
        integer(int64) :: span = 1
        !> V_BUFFER, V_MARKER, V_KEYMAP, V_SYNTAX_TABLE, V_WINDOW: the
        !> object's handle.
        type(handle_type) :: h
        !> V_STRING: text in the internal (multibyte) encoding.
        character(len=:), allocatable :: s
    end type atom_type

    !> One value.  Unallocated `a' (the default) is nil.
    type, public :: value_type
        type(atom_type), allocatable :: a(:)
    end type value_type

    !> Port of the parts of lisp.h:struct Lisp_Symbol in use.
    type, public :: symbol_type
        character(len=:), allocatable :: name
        !> The global value, valid when `bound'.
        type(value_type) :: val
        logical :: bound = .false.
        !> trapped_write == SYMBOL_NOWRITE: nil, t and keywords.
        logical :: constant = .false.
        type(value_type) :: plist
        !> The default value while the current buffer has a local value
        !> (`default_bound' false for void); see data_fm.
        type(value_type) :: default_val
        logical :: default_bound = .false.
        !> make-variable-buffer-local: setting makes the variable local.
        logical :: local_if_set = .false.
        !> SYMBOL_FORWARDED: 0 not yet known, -1 a plain variable, else
        !> the variable's number in data_fm's table of Fortran variables.
        integer :: fwd = 0
    end type symbol_type

    !> Symbol storage, indexed by id; maintained by lread_fm.
    type(symbol_type), allocatable, public :: symbols(:)
    integer(int64), public :: nsymbols = 0

    public :: nil, t_value, make_fixnum, make_string, make_symbol_ref, &
              make_buffer_ref, make_marker_ref, make_keymap_ref, make_syntax_table_ref, make_window_ref, make_list, list, cons
    public :: vtag, nilp, fixnump, symbolp, stringp, consp, listp, bufferp, &
              markerp, keymap_ref_p, syntax_table_ref_p, windowp
    public :: xfixnum, xsymbol, xstring, xhandle, list_length, list_elt, &
              list_elements, list_tail, string_intervals, set_string_intervals

contains

    ! ------------------------------------------------------------------
    ! Constructors
    ! ------------------------------------------------------------------

    !> A value made of one scalar atom.
    pure function scalar(atom) result(v)
        type(atom_type), intent(in) :: atom
        type(value_type) :: v

        allocate (v%a(1))
        v%a(1) = atom
    end function scalar

    !> The empty list / false.  Port of lisp.h:Qnil.
    pure function nil() result(v)
        type(value_type) :: v

        if (allocated(v%a)) deallocate (v%a)
    end function nil

    !> The symbol t.  Port of lisp.h:Qt.
    pure function t_value() result(v)
        type(value_type) :: v

        v = make_symbol_ref(SYM_T)
    end function t_value

    !> Port of lisp.h:make_fixnum.
    pure function make_fixnum(n) result(v)
        integer(int64), intent(in) :: n
        type(value_type) :: v
        type(atom_type) :: atom

        atom%tag = V_INT
        atom%i = n
        v = scalar(atom)
    end function make_fixnum

    !> A string value holding `text' (already in the internal encoding).
    !> Port of alloc.c:build_string.
    pure function make_string(text) result(v)
        character(len=*), intent(in) :: text
        type(value_type) :: v
        type(atom_type) :: atom

        atom%tag = V_STRING
        atom%s = text
        v = scalar(atom)
    end function make_string

    !> The symbol with id `id'; id SYM_NIL gives nil.
    !> Port of lisp.h:make_lisp_symbol.
    pure function make_symbol_ref(id) result(v)
        integer(int64), intent(in) :: id
        type(value_type) :: v
        type(atom_type) :: atom

        if (id == SYM_NIL) return
        atom%tag = V_SYMBOL
        atom%i = id
        v = scalar(atom)
    end function make_symbol_ref

    !> A value referring to buffer `h'.
    pure function make_buffer_ref(h) result(v)
        type(handle_type), intent(in) :: h
        type(value_type) :: v
        type(atom_type) :: atom

        atom%tag = V_BUFFER
        atom%h = h
        v = scalar(atom)
    end function make_buffer_ref

    !> A value referring to marker `h'.
    pure function make_marker_ref(h) result(v)
        type(handle_type), intent(in) :: h
        type(value_type) :: v
        type(atom_type) :: atom

        atom%tag = V_MARKER
        atom%h = h
        v = scalar(atom)
    end function make_marker_ref

    !> A value referring to keymap `h'.
    pure function make_keymap_ref(h) result(v)
        type(handle_type), intent(in) :: h
        type(value_type) :: v
        type(atom_type) :: atom

        atom%tag = V_KEYMAP
        atom%h = h
        v = scalar(atom)
    end function make_keymap_ref

    !> A value referring to syntax table `h' (a char-table upstream).
    pure function make_syntax_table_ref(h) result(v)
        type(handle_type), intent(in) :: h
        type(value_type) :: v
        type(atom_type) :: atom

        atom%tag = V_SYNTAX_TABLE
        atom%h = h
        v = scalar(atom)
    end function make_syntax_table_ref

    !> A value referring to window `h'.
    pure function make_window_ref(h) result(v)
        type(handle_type), intent(in) :: h
        type(value_type) :: v
        type(atom_type) :: atom

        atom%tag = V_WINDOW
        atom%h = h
        v = scalar(atom)
    end function make_window_ref

    !> The atoms of `v' as an element of a list (nil becomes an explicit
    !> V_NIL atom).
    pure subroutine element_atoms(v, atoms)
        type(value_type), intent(in) :: v
        type(atom_type), allocatable, intent(out) :: atoms(:)

        if (allocated(v%a)) then
            atoms = v%a
        else
            allocate (atoms(1))
        end if
    end subroutine element_atoms

    !> A list of `elements'; nil when there are none.  Port of alloc.c:Flist.
    !> Pass variables, not function results (see the module note).
    pure function make_list(elements) result(v)
        type(value_type), intent(in) :: elements(:)
        type(value_type) :: v
        type(atom_type), allocatable :: atoms(:), part(:)
        integer :: k

        if (size(elements) == 0) return
        allocate (atoms(1))
        atoms(1)%tag = V_LIST
        atoms(1)%i = size(elements)

        do k = 1, size(elements)
            call element_atoms(elements(k), part)
            atoms = [atoms, part]
        end do

        atoms(1)%span = size(atoms)
        call move_alloc(atoms, v%a)
    end function make_list

    !> A list of up to eight values.  Port of lisp.h:list1 ... list5 and
    !> alloc.c:Flist; safe to call with function results.
    pure function list(v1, v2, v3, v4, v5, v6, v7, v8) result(v)
        type(value_type), intent(in) :: v1
        type(value_type), intent(in), optional :: v2, v3, v4, v5, v6, v7, v8
        type(value_type) :: v
        type(value_type) :: parts(8)
        integer :: n

        n = 1
        parts(1) = v1

        if (present(v2)) then
            n = n + 1
            parts(n) = v2
        end if

        if (present(v3)) then
            n = n + 1
            parts(n) = v3
        end if

        if (present(v4)) then
            n = n + 1
            parts(n) = v4
        end if

        if (present(v5)) then
            n = n + 1
            parts(n) = v5
        end if

        if (present(v6)) then
            n = n + 1
            parts(n) = v6
        end if

        if (present(v7)) then
            n = n + 1
            parts(n) = v7
        end if

        if (present(v8)) then
            n = n + 1
            parts(n) = v8
        end if

        v = make_list(parts(1:n))
    end function list

    !> `elt' prepended to `list' (nil or a list).  Anything else would be a
    !> dotted pair, which this model does not have: `ok' is then false.
    !> Port of alloc.c:Fcons (proper lists only).
    pure subroutine cons(elt, list, v, ok)
        type(value_type), intent(in) :: elt, list
        type(value_type), intent(out) :: v
        logical, intent(out) :: ok
        type(atom_type), allocatable :: head(:)

        ok = .true.
        call element_atoms(elt, head)

        select case (vtag(list))
        case (V_NIL)
            allocate (v%a(1))
            v%a(1)%tag = V_LIST
            v%a(1)%i = 1
            v%a = [v%a, head]
        case (V_LIST)
            v%a = [list%a(1:1), head, list%a(2:)]
            v%a(1)%i = v%a(1)%i + 1
        case default
            ok = .false.
            return
        end select

        v%a(1)%span = size(v%a)
    end subroutine cons

    ! ------------------------------------------------------------------
    ! Predicates and accessors (lisp.h)
    ! ------------------------------------------------------------------

    !> Tag of `v' (V_NIL for nil).
    elemental integer(int32) function vtag(v)
        type(value_type), intent(in) :: v

        vtag = V_NIL
        if (allocated(v%a)) vtag = v%a(1)%tag
    end function vtag

    !> Port of lisp.h:NILP.
    elemental logical function nilp(v)
        type(value_type), intent(in) :: v

        nilp = vtag(v) == V_NIL
    end function nilp

    !> Port of lisp.h:FIXNUMP.
    elemental logical function fixnump(v)
        type(value_type), intent(in) :: v

        fixnump = vtag(v) == V_INT
    end function fixnump

    !> Port of lisp.h:SYMBOLP (nil is a symbol).
    elemental logical function symbolp(v)
        type(value_type), intent(in) :: v

        symbolp = vtag(v) == V_SYMBOL .or. vtag(v) == V_NIL
    end function symbolp

    !> Port of lisp.h:STRINGP.
    elemental logical function stringp(v)
        type(value_type), intent(in) :: v

        stringp = vtag(v) == V_STRING
    end function stringp

    !> A non-empty list.  Port of lisp.h:CONSP.
    elemental logical function consp(v)
        type(value_type), intent(in) :: v

        consp = vtag(v) == V_LIST
    end function consp

    !> nil or a list.  Port of data.c:Flistp.
    elemental logical function listp(v)
        type(value_type), intent(in) :: v

        listp = vtag(v) == V_LIST .or. vtag(v) == V_NIL
    end function listp

    !> Port of lisp.h:BUFFERP.
    elemental logical function bufferp(v)
        type(value_type), intent(in) :: v

        bufferp = vtag(v) == V_BUFFER
    end function bufferp

    !> Port of lisp.h:MARKERP.
    elemental logical function markerp(v)
        type(value_type), intent(in) :: v

        markerp = vtag(v) == V_MARKER
    end function markerp

    !> True for a single keymap (not a list of keymaps); see keymap_fm.
    elemental logical function keymap_ref_p(v)
        type(value_type), intent(in) :: v

        keymap_ref_p = vtag(v) == V_KEYMAP
    end function keymap_ref_p

    !> Whether `v' refers to a syntax table.
    elemental logical function syntax_table_ref_p(v)
        type(value_type), intent(in) :: v

        syntax_table_ref_p = vtag(v) == V_SYNTAX_TABLE
    end function syntax_table_ref_p

    !> Port of lisp.h:WINDOWP.
    elemental logical function windowp(v)
        type(value_type), intent(in) :: v

        windowp = vtag(v) == V_WINDOW
    end function windowp

    !> The integer in `v', unchecked.  Port of lisp.h:XFIXNUM.
    elemental integer(int64) function xfixnum(v)
        type(value_type), intent(in) :: v

        xfixnum = 0
        if (vtag(v) == V_INT) xfixnum = v%a(1)%i
    end function xfixnum

    !> The symbol id of `v' (SYM_NIL for nil), unchecked.
    !> Port of lisp.h:XSYMBOL.
    elemental integer(int64) function xsymbol(v)
        type(value_type), intent(in) :: v

        xsymbol = SYM_NIL
        if (vtag(v) == V_SYMBOL) xsymbol = v%a(1)%i
    end function xsymbol

    !> The text of string `v' ("" if not a string).  Port of lisp.h:SSDATA.
    pure function xstring(v) result(s)
        type(value_type), intent(in) :: v
        character(len=:), allocatable :: s

        if (vtag(v) == V_STRING) then
            s = v%a(1)%s
        else
            s = ""
        end if
    end function xstring

    !> The handle in a buffer, marker or keymap value.
    elemental function xhandle(v) result(h)
        type(value_type), intent(in) :: v
        type(handle_type) :: h

        if (allocated(v%a)) h = v%a(1)%h
    end function xhandle

    !> Number of elements of list `v' (0 for nil and non-lists).
    elemental integer(int64) function list_length(v)
        type(value_type), intent(in) :: v

        list_length = 0
        if (vtag(v) == V_LIST) list_length = v%a(1)%i
    end function list_length

    !> Atom index where element `k' (1-based) of list `v' starts.
    pure integer(int64) function elt_start(v, k) result(p)
        type(value_type), intent(in) :: v
        integer(int64), intent(in) :: k
        integer(int64) :: j

        p = 2

        do j = 1, k - 1
            p = p + v%a(p)%span
        end do
    end function elt_start

    !> Element `k' (1-based) of list `v'; nil if out of range.
    pure function list_elt(v, k) result(e)
        type(value_type), intent(in) :: v
        integer(int64), intent(in) :: k
        type(value_type) :: e
        integer(int64) :: p

        if (k >= 1 .and. k <= list_length(v)) then
            p = elt_start(v, k)
            if (v%a(p)%tag /= V_NIL) e%a = v%a(p:p + v%a(p)%span - 1)
        end if
    end function list_elt

    !> All elements of list `v' as an array (size 0 for nil).
    pure subroutine list_elements(v, elems)
        type(value_type), intent(in) :: v
        type(value_type), allocatable, intent(out) :: elems(:)
        integer(int64) :: k, p

        allocate (elems(list_length(v)))
        p = 2

        do k = 1, size(elems, kind=int64)
            if (v%a(p)%tag /= V_NIL) elems(k)%a = v%a(p:p + v%a(p)%span - 1)
            p = p + v%a(p)%span
        end do
    end subroutine list_elements

    !> List `v' without its first `n' elements (nil if none remain).
    !> The core of data.c:Fcdr and fns.c:Fnthcdr.
    pure function list_tail(v, n) result(tail)
        type(value_type), intent(in) :: v
        integer(int64), intent(in) :: n
        type(value_type) :: tail
        type(atom_type), allocatable :: atoms(:)
        integer(int64) :: p, len

        len = list_length(v)

        if (n <= 0) then
            tail = v
            return
        end if

        if (n >= len) return
        p = elt_start(v, n + 1)
        atoms = [v%a(1:1), v%a(p:)]
        atoms(1)%i = len - n
        atoms(1)%span = size(atoms)
        call move_alloc(atoms, tail%a)
    end function list_tail

    !> The text property intervals of string `v': the 0-based character
    !> positions [STARTS(k), ENDS(k)) and the property list of each (size 0
    !> for none, or if `v' is not a string).
    pure subroutine string_intervals(v, starts, ends, plists)
        type(value_type), intent(in) :: v
        integer(int64), allocatable, intent(out) :: starts(:), ends(:)
        type(value_type), allocatable, intent(out) :: plists(:)
        type(value_type) :: child
        integer(int64) :: k, p

        if (vtag(v) /= V_STRING) then
            allocate (starts(0), ends(0), plists(0))
            return
        end if

        allocate (starts(v%a(1)%i), ends(v%a(1)%i), plists(v%a(1)%i))
        p = 2

        do k = 1, v%a(1)%i
            child%a = v%a(p:p + v%a(p)%span - 1)
            starts(k) = xfixnum(list_elt(child, 1_int64))
            ends(k) = xfixnum(list_elt(child, 2_int64))
            plists(k) = list_tail(child, 2_int64)
            p = p + v%a(p)%span
        end do
    end subroutine string_intervals

    !> Give string `v' the text property intervals [STARTS(k), ENDS(k))
    !> with property lists PLISTS(k), replacing those it had.  Empty
    !> intervals and those with an empty plist are left out, and adjacent
    !> intervals with the same plist are kept apart (no merging here).
    pure subroutine set_string_intervals(v, starts, ends, plists)
        type(value_type), intent(inout) :: v
        integer(int64), intent(in) :: starts(:), ends(:)
        type(value_type), intent(in) :: plists(:)
        type(atom_type), allocatable :: atoms(:), part(:)
        type(value_type) :: child, head
        integer :: k

        if (vtag(v) /= V_STRING) return
        allocate (atoms(1))
        atoms(1) = v%a(1)
        atoms(1)%i = 0

        do k = 1, size(starts)
            if (ends(k) <= starts(k) .or. nilp(plists(k))) cycle
            head = list(make_fixnum(starts(k)), make_fixnum(ends(k)))
            child = head
            child%a = [head%a, plists(k)%a(2:)]
            child%a(1)%i = 2 + plists(k)%a(1)%i
            child%a(1)%span = size(child%a)
            call element_atoms(child, part)
            atoms = [atoms, part]
            atoms(1)%i = atoms(1)%i + 1
        end do

        atoms(1)%span = size(atoms)
        call move_alloc(atoms, v%a)
    end subroutine set_string_intervals

    !> Make command SELF known by its name (replacing an earlier one of
    !> the same name).
    subroutine define_command(self)
        class(command_type), target, intent(inout) :: self
        type(command_ref_type), allocatable :: grown(:)
        integer :: i

        do i = 1, n_defined_commands
            if (same_name(defined_commands(i)%p%name, self%name)) then
                defined_commands(i)%p => self
                return
            end if
        end do

        if (.not. allocated(defined_commands)) allocate (defined_commands(16))

        if (n_defined_commands == size(defined_commands)) then
            allocate (grown(2*n_defined_commands))
            grown(1:n_defined_commands) = defined_commands
            call move_alloc(grown, defined_commands)
        end if

        n_defined_commands = n_defined_commands + 1
        defined_commands(n_defined_commands)%p => self
    end subroutine define_command

    !> The command defined with `define' named NAME, or null.
    function defined_command(name) result(p)
        character(len=*), intent(in) :: name
        type(command_type), pointer :: p
        integer :: i

        p => null()

        do i = n_defined_commands, 1, -1
            if (same_name(defined_commands(i)%p%name, name)) then
                p => defined_commands(i)%p
                return
            end if
        end do
    end function defined_command

    !> Whether A and B are the same string (`==' ignores trailing blanks).
    pure logical function same_name(a, b)
        character(len=*), intent(in) :: a, b

        same_name = len(a) == len(b)
        if (same_name) same_name = a == b
    end function same_name

    !> The name of the command defined with `define' whose procedure is
    !> PROC ("" if none).
    function defined_command_name_of(proc) result(name)
        procedure(command_proc) :: proc
        character(len=:), allocatable :: name
        integer :: i

        name = ""

        do i = n_defined_commands, 1, -1
            if (.not. associated(defined_commands(i)%p%run)) cycle

            if (associated(defined_commands(i)%p%run, proc)) then
                name = defined_commands(i)%p%name
                return
            end if
        end do
    end function defined_command_name_of
end module lisp_fm
