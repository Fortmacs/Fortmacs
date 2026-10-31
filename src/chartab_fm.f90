! chartab_fm.f90 -- char-tables: values indexed by character code.
! Ported from GNU Emacs 31.1 src/chartab.c.
!
! Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H13PRO009
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

!> Char-tables: a value for every character code from 0 to `MAX_CHAR',
!> plus a default value, a parent to inherit from, a subtype (the
!> purpose) and a few extra slots.  A char-table is a registry object
!> referenced by handle (the parent is a handle too); its values are
!> integers (characters, category set numbers, ...), with `NO_VALUE' for
!> no value: an element holding it takes the default value,
!> or else the parent's.
!>
!> As upstream, the table is a trie of four levels of 64, 16, 32 and 128
!> elements; an element is either a value for all the characters it
!> covers or a sub char-table.  The sub char-tables of a table live in a
!> pool inside it (no recursive types), referenced by index; a sub
!> char-table replaced by a value goes back to the pool's free list
!> (upstream leaves it to GC).
!>
!> Deferred: Unicode property tables (`unicode_property_table_internal',
!> `get_unicode_property_internal', `put_unicode_property_internal';
!> the properties are generated Fortran tables, uni_case_fm,
!> uni_category_fm and charscript_fm, and nothing asks for them as a
!> char-table yet), `map_char_table_for_charset' (needs charsets with
!> mapping tables, step 41).
module chartab_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_WRONG_TYPE_ARGUMENT, FM_ARGS_OUT_OF_RANGE
    use handle_fm, only: handle_type, slot_table_type, null_handle
    use character_fm, only: MAX_CHAR
    implicit none(type, external)
    private

    public :: make_char_table, free_char_table, char_table_p, char_table_subtype, char_table_parent, &
              set_char_table_parent, char_table_extra_slot, set_char_table_extra_slot, char_table_extra_table, &
              set_char_table_extra_table, char_table_range, &
              set_char_table_range, char_table_ref, char_table_set, char_table_set_range, char_table_ref_and_range, &
              map_char_table, optimize_char_table, copy_char_table, check_char_table, char_table_map_proc

    !> The value of an element that holds nothing: looking
    !> it up gives the default value, or else the parent's value.
    integer(int64), parameter, public :: NO_VALUE = -huge(1_int64)

    !> Number of elements in Nth level char-table (chartab.c chartab_size).
    integer, parameter :: chartab_size(0:3) = [64, 16, 32, 128]

    !> Number of characters each element of Nth level char-table covers
    !> (chartab.c chartab_chars).
    integer, parameter :: chartab_chars(0:3) = [65536, 4096, 128, 1]

    !> Number of characters (in bits) each element of Nth level char-table
    !> covers (chartab.c chartab_bits).
    integer, parameter :: chartab_bits(0:3) = [16, 12, 7, 0]

    !> Most extra slots a char-table can have.
    integer, parameter :: MAX_EXTRA_SLOTS = 10

    !> A sub char-table of depth 1 to 3 covering the characters from
    !> `min_char' on: each element is a value, or (`sub' > 0) the index
    !> of a deeper sub char-table in the pool.
    type :: sub_char_table_type
        integer :: depth = 0
        integer :: min_char = 0
        integer(int64), allocatable :: contents(:)
        integer, allocatable :: sub(:)
    end type sub_char_table_type

    !> A char-table (struct Lisp_Char_Table).  `ascii_sub' caches the sub
    !> char-table of depth 3 holding the ASCII characters, or 0 when they
    !> all have `ascii' (upstream's ascii slot).
    type :: char_table_type
        character(len=:), allocatable :: purpose
        integer(int64) :: defalt = NO_VALUE
        type(handle_type) :: parent
        integer(int64) :: ascii = NO_VALUE
        integer :: ascii_sub = 0
        integer(int64) :: contents(0:63) = NO_VALUE
        integer :: sub(0:63) = 0
        !> The extra slots: each holds an integer (`extras') or a char-table
        !> (`extra_tables'), the other being `NO_VALUE' or the null handle.
        integer(int64), allocatable :: extras(:)
        type(handle_type), allocatable :: extra_tables(:)
        type(sub_char_table_type), allocatable :: pool(:)
        integer :: npool = 0
        integer, allocatable :: free(:)
        integer :: nfree = 0
    end type char_table_type

    abstract interface
        !> What `map_char_table' calls for each character or run of
        !> characters FROM..TO sharing the value VALUE (not `NO_VALUE');
        !> ARG is the argument given to `map_char_table'.  Not a port.
        recursive subroutine char_table_map_proc(arg, from, to, value)
            import :: handle_type, int64
            implicit none(type, external)
            type(handle_type), intent(in) :: arg
            integer, intent(in) :: from, to
            integer(int64), intent(in) :: value
        end subroutine char_table_map_proc
    end interface

    type(slot_table_type) :: registry
    type(char_table_type), allocatable :: tables(:)

contains

    ! ------------------------------------------------------------------
    ! Registry
    ! ------------------------------------------------------------------

    !> A new slot for a char-table.  Not a port.
    subroutine new_table(h)
        type(handle_type), intent(out) :: h
        type(char_table_type), allocatable :: grown(:)

        call registry%acquire(h)

        if (.not. allocated(tables)) then
            allocate (tables(registry%capacity()))
        else if (size(tables, kind=int64) < registry%capacity()) then
            allocate (grown(registry%capacity()))
            grown(1:size(tables)) = tables
            call move_alloc(grown, tables)
        end if

        tables(h%index) = char_table_type()
    end subroutine new_table

    !> The slot of char-table H, or a wrong-type-argument error
    !> (char-table-p).  Port of chartab.c:CHECK_CHAR_TABLE.
    pure subroutine check_char_table(h, i, status)
        type(handle_type), intent(in) :: h
        integer(int64), intent(out) :: i
        type(status_type), intent(out) :: status

        call registry%resolve(h, i, status)
        if (.not. status%ok()) call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(char_table_type)")
    end subroutine check_char_table

    !> Return true if OBJECT is a char-table.  Not a port (data.c
    !> char-table-p: a live handle).
    pure logical function char_table_p(object)
        type(handle_type), intent(in) :: object

        char_table_p = registry%is_live(object)
    end function char_table_p

    !> Free char-table H.  Not a port: upstream relies on GC.
    subroutine free_char_table(h, status)
        type(handle_type), intent(in) :: h
        type(status_type), intent(out) :: status
        integer(int64) :: i

        call check_char_table(h, i, status)
        if (.not. status%ok()) return
        tables(i) = char_table_type()
        call registry%release(h, status)
    end subroutine free_char_table

    !> The number of extra slots of a char-table whose purpose is PURPOSE
    !> when `make_char_table' isn't told (the `char_table_extra_slots'
    !> property upstream gives the purpose symbols): case tables have 3
    !> (casetab.c:init_casetab_once), category tables 2
    !> (category.c:init_category_once).  Not a port.
    pure integer function purpose_extra_slots(purpose) result(n)
        character(len=*), intent(in) :: purpose

        select case (purpose)
        case ("case_table")
            n = 3
        case ("category_table")
            n = 2
        case default
            n = 0
        end select
    end function purpose_extra_slots

    ! ------------------------------------------------------------------
    ! Sub char-tables
    ! ------------------------------------------------------------------

    !> A new sub char-table of DEPTH from MIN_CHAR in table I, every
    !> element DEFALT; its index in the pool.  Port of
    !> chartab.c:make_sub_char_table.
    integer function make_sub_char_table(i, depth, min_char, defalt) result(k)
        integer(int64), intent(in) :: i
        integer, intent(in) :: depth, min_char
        integer(int64), intent(in) :: defalt
        type(sub_char_table_type), allocatable :: grown(:)

        if (tables(i)%nfree > 0) then
            k = tables(i)%free(tables(i)%nfree)
            tables(i)%nfree = tables(i)%nfree - 1
        else
            if (.not. allocated(tables(i)%pool)) then
                allocate (tables(i)%pool(16))
            else if (tables(i)%npool == size(tables(i)%pool)) then
                allocate (grown(2*size(tables(i)%pool)))
                grown(1:tables(i)%npool) = tables(i)%pool
                call move_alloc(grown, tables(i)%pool)
            end if

            tables(i)%npool = tables(i)%npool + 1
            k = tables(i)%npool
        end if

        tables(i)%pool(k)%depth = depth
        tables(i)%pool(k)%min_char = min_char
        if (allocated(tables(i)%pool(k)%contents)) deallocate (tables(i)%pool(k)%contents, tables(i)%pool(k)%sub)
        allocate (tables(i)%pool(k)%contents(chartab_size(depth)), tables(i)%pool(k)%sub(chartab_size(depth)))
        tables(i)%pool(k)%contents = defalt
        tables(i)%pool(k)%sub = 0
    end function make_sub_char_table

    !> Give sub char-table K of table I, and the deeper ones it holds,
    !> back to the pool's free list.  Not a port: upstream leaves them to
    !> GC.
    recursive subroutine release_sub(i, k)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k
        integer, allocatable :: grown(:)
        integer :: j

        do j = 1, size(tables(i)%pool(k)%sub)
            if (tables(i)%pool(k)%sub(j) > 0) call release_sub(i, tables(i)%pool(k)%sub(j))
        end do

        tables(i)%pool(k)%sub = 0

        if (.not. allocated(tables(i)%free)) then
            allocate (tables(i)%free(16))
        else if (tables(i)%nfree == size(tables(i)%free)) then
            allocate (grown(2*size(tables(i)%free)))
            grown(1:tables(i)%nfree) = tables(i)%free
            call move_alloc(grown, tables(i)%free)
        end if

        tables(i)%nfree = tables(i)%nfree + 1
        tables(i)%free(tables(i)%nfree) = k
    end subroutine release_sub

    !> Set element IDX (0-based) of sub char-table K of table I (the top
    !> level when K is 0) to the value VAL, releasing a sub char-table it
    !> held.  Not a port (set_char_table_contents and
    !> set_sub_char_table_contents with a value).
    recursive subroutine set_element(i, k, idx, val)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k, idx
        integer(int64), intent(in) :: val
        integer :: s

        if (k == 0) then
            s = tables(i)%sub(idx)
            tables(i)%sub(idx) = 0
            tables(i)%contents(idx) = val
        else
            s = tables(i)%pool(k)%sub(idx + 1)
            tables(i)%pool(k)%sub(idx + 1) = 0
            tables(i)%pool(k)%contents(idx + 1) = val
        end if

        if (s > 0) call release_sub(i, s)
    end subroutine set_element

    !> Element IDX (0-based) of sub char-table K of table I (the top level
    !> when K is 0): its value VAL, or the sub char-table S it holds
    !> (0 for a value).  Not a port.
    pure subroutine get_element(i, k, idx, val, s)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k, idx
        integer(int64), intent(out) :: val
        integer, intent(out) :: s

        if (k == 0) then
            val = tables(i)%contents(idx)
            s = tables(i)%sub(idx)
        else
            val = tables(i)%pool(k)%contents(idx + 1)
            s = tables(i)%pool(k)%sub(idx + 1)
        end if
    end subroutine get_element

    !> The index of element C of a sub char-table of DEPTH from MIN_CHAR.
    !> Port of chartab.c CHARTAB_IDX.
    pure integer function chartab_idx(c, depth, min_char)
        integer, intent(in) :: c, depth, min_char

        chartab_idx = ishft(c - min_char, -chartab_bits(depth))
    end function chartab_idx

    !> Recompute the ASCII cache of table I: the sub char-table of depth 3
    !> holding the ASCII characters, or their common value.  Port of
    !> chartab.c:char_table_ascii and chartab.c:set_char_table_ascii.
    subroutine set_char_table_ascii(i)
        integer(int64), intent(in) :: i
        integer(int64) :: val
        integer :: s

        tables(i)%ascii_sub = 0
        call get_element(i, 0, 0, val, s)

        if (s > 0) then
            call get_element(i, s, 0, val, s)

            if (s > 0) then
                call get_element(i, s, 0, val, s)

                if (s > 0) then
                    tables(i)%ascii_sub = s
                    val = NO_VALUE
                end if
            end if
        end if

        tables(i)%ascii = val
    end subroutine set_char_table_ascii

    ! ------------------------------------------------------------------
    ! The table
    ! ------------------------------------------------------------------

    !> Return a newly created char-table, with purpose PURPOSE.
    !> Each element is initialized to INIT, which defaults to `NO_VALUE'.
    !>
    !> PURPOSE names the subtype.  N_EXTRAS, from 0 to 10, specifies how
    !> many extra slots the char-table has; it defaults to 3 for
    !> "case_table", 2 for "category_table" and 0 for other purposes.
    !> Port of chartab.c:Fmake_char_table (N_EXTRAS stands for the
    !> purpose symbol's `char_table_extra_slots' property).
    subroutine make_char_table(h, purpose, status, init, n_extras)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: purpose
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: init
        integer, intent(in), optional :: n_extras
        integer(int64) :: val
        integer :: n

        status = status_type()
        val = NO_VALUE
        if (present(init)) val = init
        n = purpose_extra_slots(purpose)
        if (present(n_extras)) n = n_extras

        if (n < 0 .or. n > MAX_EXTRA_SLOTS) then
            call set_error(status, FM_ARGS_OUT_OF_RANGE, "char_table_extra_slots")
            h = null_handle()
            return
        end if

        call new_table(h)

        associate (t => tables(h%index))
            t%purpose = purpose
            t%defalt = val
            t%ascii = val
            t%contents = val
            allocate (t%extras(n), t%extra_tables(n))
            t%extras = val
        end associate
    end subroutine make_char_table

    !> Return a copy of char-table TABLE: its sub char-tables are copied,
    !> the values and the parent are shared.  Port of
    !> chartab.c:copy_char_table and chartab.c:copy_sub_char_table (assignment
    !> copies the pool).
    subroutine copy_char_table(table, copy, status)
        type(handle_type), intent(in) :: table
        type(handle_type), intent(out) :: copy
        type(status_type), intent(out) :: status
        integer(int64) :: i
        type(char_table_type) :: tmp

        call check_char_table(table, i, status)

        if (.not. status%ok()) then
            copy = null_handle()
            return
        end if

        tmp = tables(i)
        call new_table(copy)
        call move_alloc_table(tmp, tables(copy%index))
    end subroutine copy_char_table

    !> Move char-table FROM into TO.  Not a port.
    subroutine move_alloc_table(from, to)
        type(char_table_type), intent(inout) :: from
        type(char_table_type), intent(inout) :: to

        to = char_table_type()
        call move_alloc(from%purpose, to%purpose)
        to%defalt = from%defalt
        to%parent = from%parent
        to%ascii = from%ascii
        to%ascii_sub = from%ascii_sub
        to%contents = from%contents
        to%sub = from%sub
        call move_alloc(from%extras, to%extras)
        call move_alloc(from%extra_tables, to%extra_tables)
        if (allocated(from%pool)) call move_alloc(from%pool, to%pool)
        to%npool = from%npool
        if (allocated(from%free)) call move_alloc(from%free, to%free)
        to%nfree = from%nfree
    end subroutine move_alloc_table

    !> Return the subtype of char-table CHAR_TABLE.  The value is a name.
    !> Port of chartab.c:Fchar_table_subtype.
    function char_table_subtype(char_table, status) result(purpose)
        type(handle_type), intent(in) :: char_table
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: purpose
        integer(int64) :: i

        call check_char_table(char_table, i, status)

        if (.not. status%ok()) then
            purpose = ""
            return
        end if

        purpose = tables(i)%purpose
    end function char_table_subtype

    !> Return the parent char-table of CHAR_TABLE.
    !> The value is either the null handle or another char-table.
    !> If CHAR_TABLE holds `NO_VALUE' for a given character,
    !> then the actual applicable value is inherited from the parent char-table
    !> (or from its parents, if necessary).  Port of
    !> chartab.c:Fchar_table_parent.
    function char_table_parent(char_table, status) result(parent)
        type(handle_type), intent(in) :: char_table
        type(status_type), intent(out) :: status
        type(handle_type) :: parent
        integer(int64) :: i

        parent = null_handle()
        call check_char_table(char_table, i, status)
        if (.not. status%ok()) return
        if (registry%is_live(tables(i)%parent)) parent = tables(i)%parent
    end function char_table_parent

    !> Set the parent char-table of CHAR_TABLE to PARENT.
    !> PARENT must be either the null handle or another char-table.
    !> Port of chartab.c:Fset_char_table_parent and chartab.c:set_char_table_parent.
    subroutine set_char_table_parent(char_table, parent, status)
        type(handle_type), intent(in) :: char_table, parent
        type(status_type), intent(out) :: status
        integer(int64) :: i, j
        type(handle_type) :: temp

        call check_char_table(char_table, i, status)
        if (.not. status%ok()) return

        if (parent%index /= 0) then
            call check_char_table(parent, j, status)
            if (.not. status%ok()) return
            temp = parent

            do while (registry%is_live(temp))
                if (temp%index == char_table%index .and. temp%generation == char_table%generation) then
                    call set_error(status, FM_GENERIC_ERROR, "Attempt to make a chartable be its own parent")
                    return
                end if

                temp = tables(temp%index)%parent
            end do
        end if

        tables(i)%parent = parent
    end subroutine set_char_table_parent

    !> Return the value of CHAR_TABLE's extra-slot number N.  Port of
    !> chartab.c:Fchar_table_extra_slot (N counts from 0).
    integer(int64) function char_table_extra_slot(char_table, n, status) result(value)
        type(handle_type), intent(in) :: char_table
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        integer(int64) :: i

        value = NO_VALUE
        call check_char_table(char_table, i, status)
        if (.not. status%ok()) return

        if (n < 0 .or. n >= size(tables(i)%extras)) then
            call set_error(status, FM_ARGS_OUT_OF_RANGE, "char_table_extra_slot")
            return
        end if

        value = tables(i)%extras(n + 1)
    end function char_table_extra_slot

    !> Set CHAR_TABLE's extra-slot number N to VALUE.  Port of
    !> chartab.c:Fset_char_table_extra_slot (N counts from 0).
    subroutine set_char_table_extra_slot(char_table, n, value, status)
        type(handle_type), intent(in) :: char_table
        integer, intent(in) :: n
        integer(int64), intent(in) :: value
        type(status_type), intent(out) :: status
        integer(int64) :: i

        call check_char_table(char_table, i, status)
        if (.not. status%ok()) return

        if (n < 0 .or. n >= size(tables(i)%extras)) then
            call set_error(status, FM_ARGS_OUT_OF_RANGE, "set_char_table_extra_slot")
            return
        end if

        tables(i)%extras(n + 1) = value
        tables(i)%extra_tables(n + 1) = null_handle()
    end subroutine set_char_table_extra_slot

    !> Return the char-table in CHAR_TABLE's extra-slot number N, or the
    !> null handle when it holds none.  Port of
    !> chartab.c:Fchar_table_extra_slot for a slot holding a char-table.
    function char_table_extra_table(char_table, n, status) result(value)
        type(handle_type), intent(in) :: char_table
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        type(handle_type) :: value
        integer(int64) :: i

        value = null_handle()
        call check_char_table(char_table, i, status)
        if (.not. status%ok()) return

        if (n < 0 .or. n >= size(tables(i)%extras)) then
            call set_error(status, FM_ARGS_OUT_OF_RANGE, "char_table_extra_slot")
            return
        end if

        value = tables(i)%extra_tables(n + 1)
    end function char_table_extra_table

    !> Set CHAR_TABLE's extra-slot number N to the char-table VALUE (the
    !> null handle to empty it).  Port of
    !> chartab.c:Fset_char_table_extra_slot for a char-table value.
    subroutine set_char_table_extra_table(char_table, n, value, status)
        type(handle_type), intent(in) :: char_table
        integer, intent(in) :: n
        type(handle_type), intent(in) :: value
        type(status_type), intent(out) :: status
        integer(int64) :: i

        call check_char_table(char_table, i, status)
        if (.not. status%ok()) return

        if (n < 0 .or. n >= size(tables(i)%extras)) then
            call set_error(status, FM_ARGS_OUT_OF_RANGE, "set_char_table_extra_slot")
            return
        end if

        tables(i)%extras(n + 1) = NO_VALUE
        tables(i)%extra_tables(n + 1) = value
    end subroutine set_char_table_extra_table

    ! ------------------------------------------------------------------
    ! Lookup
    ! ------------------------------------------------------------------

    !> The value for C in sub char-table K of table I, without the default
    !> value or the parent.  Port of chartab.c:sub_char_table_ref.
    pure recursive integer(int64) function sub_char_table_ref(i, k, c) result(val)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k, c
        integer :: s

        call get_element(i, k, chartab_idx(c, tables(i)%pool(k)%depth, tables(i)%pool(k)%min_char), val, s)
        if (s > 0) val = sub_char_table_ref(i, s, c)
    end function sub_char_table_ref

    !> The value for C in table I, without the default value or the
    !> parent.  Not a port (char_table_ref's first part).
    pure integer(int64) function own_ref(i, c) result(val)
        integer(int64), intent(in) :: i
        integer, intent(in) :: c
        integer :: s

        if (c < 128) then
            val = tables(i)%ascii
            if (tables(i)%ascii_sub > 0) val = tables(i)%pool(tables(i)%ascii_sub)%contents(c + 1)
        else
            call get_element(i, 0, chartab_idx(c, 0, 0), val, s)
            if (s > 0) val = sub_char_table_ref(i, s, c)
        end if
    end function own_ref

    !> The value for character C in char-table TABLE: its own, else the
    !> default value, else the parent's; `NO_VALUE' if none.  An invalid
    !> TABLE has no values.  Port of chartab.c:char_table_ref
    !> (CHAR_TABLE_REF).
    pure recursive integer(int64) function char_table_ref(table, c) result(val)
        type(handle_type), intent(in) :: table
        integer, intent(in) :: c
        integer(int64) :: i
        type(status_type) :: status

        val = NO_VALUE
        call check_char_table(table, i, status)
        if (.not. status%ok()) return
        if (c < 0 .or. c > MAX_CHAR) return
        val = own_ref(i, c)

        if (val == NO_VALUE) then
            val = tables(i)%defalt
            if (val == NO_VALUE .and. registry%is_live(tables(i)%parent)) val = char_table_ref(tables(i)%parent, c)
        end if
    end function char_table_ref

    !> Element IDX of sub char-table K of table I (the top level when K is
    !> 0) for C, with DEFALT for `NO_VALUE', shrinking FROM..TO as for
    !> `char_table_ref_and_range'.  Port of chartab.c:char_table_ref_simple.
    recursive integer(int64) function char_table_ref_simple(i, k, idx, c, from, to, defalt) result(val)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k, idx, c
        integer, intent(inout) :: from, to
        integer(int64), intent(in) :: defalt
        integer :: s

        call get_element(i, k, idx, val, s)

        if (s > 0) then
            val = sub_char_table_ref_and_range(i, s, c, from, to, defalt)
        else if (val == NO_VALUE) then
            val = defalt
        end if
    end function char_table_ref_simple

    !> Port of chartab.c:sub_char_table_ref_and_range.
    recursive integer(int64) function sub_char_table_ref_and_range(i, k, c, from, to, defalt) result(val)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k, c
        integer, intent(inout) :: from, to
        integer(int64), intent(in) :: defalt
        integer :: depth, min_char, idx, cidx, cc
        integer(int64) :: this_val

        depth = tables(i)%pool(k)%depth
        min_char = tables(i)%pool(k)%min_char
        cidx = chartab_idx(c, depth, min_char)
        val = char_table_ref_simple(i, k, cidx, c, from, to, defalt)
        idx = cidx

        do while (idx > 0 .and. from < min_char + idx*chartab_chars(depth))
            cc = min_char + idx*chartab_chars(depth) - 1
            idx = idx - 1
            this_val = char_table_ref_simple(i, k, idx, cc, from, to, defalt)

            if (this_val /= val) then
                from = cc + 1
                exit
            end if
        end do

        do
            cc = (cidx + 1)*chartab_chars(depth)
            if (cc >= chartab_chars(depth - 1)) exit
            cc = cc + min_char
            if (cc > to) exit
            cidx = cidx + 1
            this_val = char_table_ref_simple(i, k, cidx, cc, from, to, defalt)

            if (this_val /= val) then
                to = cc - 1
                exit
            end if
        end do
    end function sub_char_table_ref_and_range

    !> Return the value for C in char-table TABLE.  Shrink the range FROM
    !> and TO to cover characters (containing C) that have the same value
    !> as C.  It is not assured that the values of (FROM - 1) and (TO +
    !> 1) are different from that of C.  A negative FROM is 0, a negative
    !> TO `MAX_CHAR'.  The parent is not consulted.  Port of
    !> chartab.c:char_table_ref_and_range.
    integer(int64) function char_table_ref_and_range(table, c, from, to) result(val)
        type(handle_type), intent(in) :: table
        integer, intent(in) :: c
        integer, intent(inout) :: from, to
        integer(int64) :: i, this_val, defalt
        integer :: cidx, idx, cc
        type(status_type) :: status

        val = NO_VALUE
        call check_char_table(table, i, status)
        if (.not. status%ok()) return
        if (from < 0) from = 0
        if (to < 0) to = MAX_CHAR
        defalt = tables(i)%defalt
        cidx = chartab_idx(c, 0, 0)
        val = char_table_ref_simple(i, 0, cidx, c, from, to, defalt)
        idx = cidx

        do while (from < idx*chartab_chars(0))
            cc = idx*chartab_chars(0) - 1
            idx = idx - 1
            this_val = char_table_ref_simple(i, 0, idx, cc, from, to, defalt)

            if (this_val /= val) then
                from = cc + 1
                exit
            end if
        end do

        do while (to >= (cidx + 1)*chartab_chars(0))
            cidx = cidx + 1
            cc = cidx*chartab_chars(0)
            this_val = char_table_ref_simple(i, 0, cidx, cc, from, to, defalt)

            if (this_val /= val) then
                to = cc - 1
                exit
            end if
        end do
    end function char_table_ref_and_range

    ! ------------------------------------------------------------------
    ! Setting
    ! ------------------------------------------------------------------

    !> Port of chartab.c:sub_char_table_set.
    recursive subroutine sub_char_table_set(i, k, c, val)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k, c
        integer(int64), intent(in) :: val
        integer :: depth, min_char, idx, s
        integer(int64) :: old

        depth = tables(i)%pool(k)%depth
        min_char = tables(i)%pool(k)%min_char
        idx = chartab_idx(c, depth, min_char)

        if (depth == 3) then
            call set_element(i, k, idx, val)
        else
            call get_element(i, k, idx, old, s)

            if (s == 0) then
                s = make_sub_char_table(i, depth + 1, min_char + idx*chartab_chars(depth), old)
                tables(i)%pool(k)%sub(idx + 1) = s
            end if

            call sub_char_table_set(i, s, c, val)
        end if
    end subroutine sub_char_table_set

    !> Set the value for character C in char-table TABLE to VAL.  Port of
    !> chartab.c:char_table_set (CHAR_TABLE_SET).
    subroutine char_table_set(table, c, val, status)
        type(handle_type), intent(in) :: table
        integer, intent(in) :: c
        integer(int64), intent(in) :: val
        type(status_type), intent(out) :: status
        integer(int64) :: i, old
        integer :: idx, s

        call check_char_table(table, i, status)
        if (.not. status%ok()) return

        if (c < 0 .or. c > MAX_CHAR) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer (a character code)")
            return
        end if

        if (c < 128 .and. tables(i)%ascii_sub > 0) then
            call set_element(i, tables(i)%ascii_sub, c, val)
        else
            idx = chartab_idx(c, 0, 0)
            call get_element(i, 0, idx, old, s)

            if (s == 0) then
                s = make_sub_char_table(i, 1, idx*chartab_chars(0), old)
                tables(i)%sub(idx) = s
            end if

            call sub_char_table_set(i, s, c, val)
            if (c < 128) call set_char_table_ascii(i)
        end if
    end subroutine char_table_set

    !> Port of chartab.c:sub_char_table_set_range.
    recursive subroutine sub_char_table_set_range(i, k, from, to, val)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k, from, to
        integer(int64), intent(in) :: val
        integer :: depth, min_char, chars_in_block, idx, c, lim, s, start
        integer(int64) :: old

        depth = tables(i)%pool(k)%depth
        min_char = tables(i)%pool(k)%min_char
        chars_in_block = chartab_chars(depth)
        lim = chartab_size(depth)
        start = max(from, min_char)
        idx = chartab_idx(start, depth, min_char)
        c = min_char + chars_in_block*idx

        do while (idx < lim)
            if (c > to) exit

            if (start <= c .and. c + chars_in_block - 1 <= to) then
                call set_element(i, k, idx, val)
            else
                call get_element(i, k, idx, old, s)

                if (s == 0) then
                    s = make_sub_char_table(i, depth + 1, c, old)
                    tables(i)%pool(k)%sub(idx + 1) = s
                end if

                call sub_char_table_set_range(i, s, start, to, val)
            end if

            idx = idx + 1
            c = c + chars_in_block
        end do
    end subroutine sub_char_table_set_range

    !> Set the value for the characters FROM to TO in char-table TABLE to
    !> VAL.  A reversed range sets nothing.  Port of
    !> chartab.c:char_table_set_range.
    subroutine char_table_set_range(table, from, to, val, status)
        type(handle_type), intent(in) :: table
        integer, intent(in) :: from, to
        integer(int64), intent(in) :: val
        type(status_type), intent(out) :: status
        integer(int64) :: i, old
        integer :: idx, lim, c, s

        if (from == to) then
            call char_table_set(table, from, val, status)
            return
        end if

        call check_char_table(table, i, status)
        if (.not. status%ok()) return

        if (from < 0 .or. from > MAX_CHAR .or. to < 0 .or. to > MAX_CHAR) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer (a character code)")
            return
        end if

        lim = chartab_idx(to, 0, 0)
        idx = chartab_idx(from, 0, 0)
        c = idx*chartab_chars(0)

        do while (idx <= lim)
            if (c > to) exit

            if (from <= c .and. c + chartab_chars(0) - 1 <= to) then
                call set_element(i, 0, idx, val)
            else
                call get_element(i, 0, idx, old, s)

                if (s == 0) then
                    s = make_sub_char_table(i, 1, idx*chartab_chars(0), old)
                    tables(i)%sub(idx) = s
                end if

                call sub_char_table_set_range(i, s, from, to, val)
            end if

            idx = idx + 1
            c = c + chartab_chars(0)
        end do

        if (from < 128) call set_char_table_ascii(i)
    end subroutine char_table_set_range

    !> Return the value in CHAR_TABLE for a range of characters RANGE.
    !> RANGE should be absent (for the default value),
    !> FROM and TO (for characters in the range), or FROM alone (a character code).
    !> If RANGE is FROM..TO, the function returns the value for FROM.
    !> Port of chartab.c:Fchar_table_range.
    integer(int64) function char_table_range(char_table, status, from, to) result(val)
        type(handle_type), intent(in) :: char_table
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: from, to
        integer(int64) :: i
        integer :: f, t

        val = NO_VALUE
        call check_char_table(char_table, i, status)
        if (.not. status%ok()) return

        if (.not. present(from)) then
            val = tables(i)%defalt
            return
        end if

        if (from < 0 .or. from > MAX_CHAR) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer (a character code)")
            return
        end if

        if (.not. present(to)) then
            val = char_table_ref(char_table, from)
            return
        end if

        if (to < 0 .or. to > MAX_CHAR) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer (a character code)")
            return
        end if

        f = from
        t = to
        val = char_table_ref_and_range(char_table, from, f, t)
    end function char_table_range

    !> Set the value in CHAR_TABLE for a range of characters RANGE to VALUE.
    !> RANGE should be ALL true (for all characters), absent (for the default value),
    !> FROM and TO (for characters in the range),
    !> or FROM alone (a character code).  Port of chartab.c:Fset_char_table_range.
    subroutine set_char_table_range(char_table, value, status, from, to, all)
        type(handle_type), intent(in) :: char_table
        integer(int64), intent(in) :: value
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: from, to
        logical, intent(in), optional :: all
        integer(int64) :: i
        integer :: j
        logical :: every

        call check_char_table(char_table, i, status)
        if (.not. status%ok()) return
        every = .false.
        if (present(all)) every = all

        if (every) then
            tables(i)%ascii = value
            tables(i)%ascii_sub = 0

            do j = 0, chartab_size(0) - 1
                call set_element(i, 0, j, value)
            end do
        else if (.not. present(from)) then
            tables(i)%defalt = value
        else if (.not. present(to)) then
            call char_table_set(char_table, from, value, status)
        else
            call char_table_set_range(char_table, from, to, value, status)
        end if
    end subroutine set_char_table_range

    ! ------------------------------------------------------------------
    ! Optimizing
    ! ------------------------------------------------------------------

    !> Optimize sub char-table K of table I: when all its elements are the
    !> same value, return true with that value in ELT.  Port of
    !> chartab.c:optimize_sub_char_table (values compare by ==).
    recursive logical function optimize_sub_char_table(i, k, elt) result(optimizable)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k
        integer(int64), intent(out) :: elt
        integer(int64) :: this
        integer :: j, s

        optimizable = .true.

        do j = 0, chartab_size(tables(i)%pool(k)%depth) - 1
            call get_element(i, k, j, this, s)

            if (s > 0) then
                if (optimize_sub_char_table(i, s, this)) call set_element(i, k, j, this)
                call get_element(i, k, j, this, s)
            end if

            if (j == 0) then
                elt = this
                optimizable = s == 0
            else if (optimizable) then
                optimizable = s == 0
                if (optimizable) optimizable = this == elt
            end if
        end do
    end function optimize_sub_char_table

    !> Optimize CHAR_TABLE: merge sub char-tables whose entries are all
    !> equal.  Port of chartab.c:Foptimize_char_table (no TEST: the values
    !> are integers, equal when ==).
    subroutine optimize_char_table(char_table, status)
        type(handle_type), intent(in) :: char_table
        type(status_type), intent(out) :: status
        integer(int64) :: i, elt, this
        integer :: j, s

        call check_char_table(char_table, i, status)
        if (.not. status%ok()) return

        do j = 0, chartab_size(0) - 1
            call get_element(i, 0, j, this, s)

            if (s > 0) then
                if (optimize_sub_char_table(i, s, elt)) call set_element(i, 0, j, elt)
            end if
        end do

        ! Reset the `ascii' cache, in case it got optimized away.
        call set_char_table_ascii(i)
    end subroutine optimize_char_table

    ! ------------------------------------------------------------------
    ! Mapping
    ! ------------------------------------------------------------------

    !> Map PROC over sub char-table K of table I (the top level when K is
    !> 0), calling it for each character or group of characters that
    !> share a value.  RFROM..RTO is the range of target characters, VAL
    !> the value of RFROM in the table.
    !>
    !> VAL becomes the value of the last character covered by the table
    !> (not the value inherited from the parent), and RFROM the minimum
    !> character C where C and all the following characters in the table
    !> have the same value.  Port of chartab.c:map_sub_char_table.
    recursive subroutine map_sub_char_table(proc, arg, i, k, val, rfrom, rto)
        procedure(char_table_map_proc) :: proc
        type(handle_type), intent(in) :: arg
        integer(int64), intent(in) :: i
        integer, intent(in) :: k
        integer(int64), intent(inout) :: val
        integer, intent(inout) :: rfrom, rto
        integer :: depth, min_char, last_char, chars_in_block, from, to, idx, c, nextc, s
        integer(int64) :: this, p
        logical :: different_value
        type(status_type) :: status

        from = rfrom
        to = rto

        if (k > 0) then
            depth = tables(i)%pool(k)%depth
            min_char = tables(i)%pool(k)%min_char
            last_char = min_char + chartab_chars(depth - 1) - 1
        else
            depth = 0
            min_char = 0
            last_char = MAX_CHAR
        end if

        chars_in_block = chartab_chars(depth)
        if (to < last_char) last_char = to
        ! Set IDX to the index of the first element to check.
        idx = 0
        if (from > min_char) idx = (from - min_char)/chars_in_block
        c = min_char + chars_in_block*idx

        do while (c <= last_char)
            call get_element(i, k, idx, this, s)
            nextc = c + chars_in_block

            if (s > 0) then
                if (to >= nextc) rto = nextc - 1
                call map_sub_char_table(proc, arg, i, s, val, rfrom, rto)
            else
                if (this == NO_VALUE) this = tables(i)%defalt

                if (val /= this) then
                    different_value = .true.

                    if (val == NO_VALUE .and. registry%is_live(tables(i)%parent)) then
                        call check_char_table(tables(i)%parent, p, status)
                        ! The value of FROM in the parent without checking
                        ! the parent of the parent.
                        val = own_ref(p, from)
                        if (val == NO_VALUE) val = tables(p)%defalt
                        rto = c - 1
                        call map_sub_char_table(proc, arg, p, 0, val, rfrom, rto)
                        if (val == this) different_value = .false.
                    end if

                    if (val /= NO_VALUE .and. different_value) then
                        rto = c - 1
                        call proc(arg, rfrom, rto, val)
                    end if

                    val = this
                    from = c
                    rfrom = c
                end if
            end if

            rto = to
            idx = idx + 1
            c = c + chars_in_block
        end do
    end subroutine map_sub_char_table

    !> Call PROC for each character in TABLE that has a value: with ARG,
    !> the characters FROM..TO (FROM = TO for one character) of a run that
    !> shares a value, and that value (what `char_table_range' returns for
    !> the run).  Runs follow each other in character order, and where
    !> TABLE has no value its parents are consulted.  Port of
    !> chartab.c:map_char_table, which `Fmap_char_table' calls with ARG
    !> TABLE (chartab.c:Fmap_char_table).
    recursive subroutine map_char_table(proc, table, status, arg)
        procedure(char_table_map_proc) :: proc
        type(handle_type), intent(in) :: table
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: arg
        type(handle_type) :: a, parent
        integer(int64) :: i, val
        integer :: rfrom, rto

        call check_char_table(table, i, status)
        if (.not. status%ok()) return
        a = table
        if (present(arg)) a = arg
        rfrom = 0
        rto = MAX_CHAR
        val = tables(i)%ascii
        if (tables(i)%ascii_sub > 0) val = tables(i)%pool(tables(i)%ascii_sub)%contents(1)
        call map_sub_char_table(proc, a, i, 0, val, rfrom, rto)

        ! If VAL is unset and TABLE has a parent, we must consult the parent
        ! recursively.
        do while (val == NO_VALUE .and. registry%is_live(tables(i)%parent))
            parent = tables(i)%parent
            call check_char_table(parent, i, status)
            ! The value of FROM in the parent without checking the parent
            ! of the parent.
            val = own_ref(i, rfrom)
            if (val == NO_VALUE) val = tables(i)%defalt
            call map_sub_char_table(proc, a, i, 0, val, rfrom, rto)
        end do

        status = status_type()
        if (val /= NO_VALUE) call proc(a, rfrom, rto, val)
    end subroutine map_char_table
end module chartab_fm
