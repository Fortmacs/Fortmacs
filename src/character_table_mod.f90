! character_table_mod.f90 -- char-tables: values indexed by character code.
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

!> Char-tables: a value for every character code from 0 to `MAXIMUM_CHARACTER',
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
!> the properties are generated Fortran tables, unicode_case_mod,
!> unicode_category_mod and character_script_mod, and nothing asks for them as a
!> char-table yet), `map_character_table_for_charset' (needs charsets with
!> mapping tables, step 41).
module character_table_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, WRONG_TYPE_ARGUMENT_CODE, ARGUMENTS_OUT_OF_RANGE_CODE
    use handle_mod, only: handle_type, slot_table_type, null_handle
    use character_mod, only: MAXIMUM_CHARACTER
    implicit none(type, external)
    private

    public :: make_character_table, destroy_character_table, is_character_table, character_table_subtype, character_table_parent, &
              set_character_table_parent, character_table_extra_slot, set_character_table_extra_slot, character_table_extra_table, &
              set_character_table_extra_table, character_table_range, &
              set_character_table_range, character_table_reference, character_table_set, character_table_set_range, &
              character_table_reference_and_range, &
              map_character_table, optimize_character_table, copy_character_table, check_character_table, &
              map_character_table_procedure

    !> The value of an element that holds nothing: looking
    !> it up gives the default value, or else the parent's value.
    integer(int64), parameter, public :: NO_VALUE = -huge(1_int64)

    !> Number of elements in Nth level char-table (chartab.c chartab_size).
    integer, parameter :: chartab_size(0:3) = [64, 16, 32, 128]

    !> Number of characters each element of Nth level char-table covers
    !> (chartab.c chartab_characters).
    integer, parameter :: chartab_characters(0:3) = [65536, 4096, 128, 1]

    !> Number of characters (in bits) each element of Nth level char-table
    !> covers (chartab.c chartab_bits).
    integer, parameter :: chartab_bits(0:3) = [16, 12, 7, 0]

    !> Most extra slots a char-table can have.
    integer, parameter :: MAXIMUM_EXTRA_SLOTS = 10

    !> A sub char-table of depth 1 to 3 covering the characters from
    !> `minimum_character' on: each element is a value, or (`sub' > 0) the index
    !> of a deeper sub char-table in the pool.
    type :: sub_character_table_type
        integer :: depth = 0
        integer :: minimum_character = 0
        integer(int64), allocatable :: contents(:)
        integer, allocatable :: sub(:)
    end type sub_character_table_type

    !> A char-table (struct Lisp_Character_Table).  `ascii_sub' caches the sub
    !> char-table of depth 3 holding the ASCII characters, or 0 when they
    !> all have `ascii' (upstream's ascii slot).
    type :: character_table_type
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
        type(sub_character_table_type), allocatable :: pool(:)
        integer :: npool = 0
        integer, allocatable :: free(:)
        integer :: nfree = 0
    end type character_table_type

    abstract interface
        !> What `map_character_table' calls for each character or run of
        !> characters FROM..TO sharing the value VALUE (not `NO_VALUE');
        !> ARGUMENT is the argument given to `map_character_table'.  Not a port.
        recursive subroutine map_character_table_procedure(argument, from, to, value)
            import :: handle_type, int64
            implicit none(type, external)
            type(handle_type), intent(in) :: argument
            integer, intent(in) :: from, to
            integer(int64), intent(in) :: value
        end subroutine map_character_table_procedure
    end interface

    type(slot_table_type) :: registry
    type(character_table_type), allocatable :: tables(:)

contains

    ! ------------------------------------------------------------------
    ! Registry
    ! ------------------------------------------------------------------

    !> A new slot for a char-table.  Not a port.
    subroutine make_table(h)
        type(handle_type), intent(out) :: h
        type(character_table_type), allocatable :: grown(:)

        call registry%acquire(h)

        if (.not. allocated(tables)) then
            allocate (tables(registry%capacity()))
        else if (size(tables, kind=int64) < registry%capacity()) then
            allocate (grown(registry%capacity()))
            grown(1:size(tables)) = tables
            call move_alloc(grown, tables)
        end if

        tables(h%index) = character_table_type()
    end subroutine make_table

    !> The slot of char-table H, or a `wrong_type_argument' error
    !> (char-table-p).  Port of chartab.c:CHECK_CHAR_TABLE.
    pure subroutine check_character_table(h, i, status)
        type(handle_type), intent(in) :: h
        integer(int64), intent(out) :: i
        type(status_type), intent(out) :: status

        call registry%resolve(h, i, status)
        if (.not. status%ok()) call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected type(character_table_type)")
    end subroutine check_character_table

    !> Return true if OBJECT is a char-table.  Not a port (data.c
    !> char-table-p: a live handle).
    pure logical function is_character_table(object)
        type(handle_type), intent(in) :: object

        is_character_table = registry%is_live(object)
    end function is_character_table

    !> Free char-table H.  Not a port: upstream relies on GC.
    subroutine destroy_character_table(h, status)
        type(handle_type), intent(in) :: h
        type(status_type), intent(out) :: status
        integer(int64) :: i

        call check_character_table(h, i, status)
        if (.not. status%ok()) return
        tables(i) = character_table_type()
        call registry%release(h, status)
    end subroutine destroy_character_table

    !> The number of extra slots of a char-table whose purpose is PURPOSE
    !> when `make_character_table' isn't told (the `character_table_extra_slots'
    !> property upstream gives the purpose names): case tables have 3
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

    !> A new sub char-table of DEPTH from MINIMUM_CHARACTER in table I, every
    !> element DEFALT; its index in the pool.  Port of
    !> chartab.c:make_sub_char_table.
    integer function make_sub_character_table(i, depth, minimum_character, defalt) result(k)
        integer(int64), intent(in) :: i
        integer, intent(in) :: depth, minimum_character
        integer(int64), intent(in) :: defalt
        type(sub_character_table_type), allocatable :: grown(:)

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
        tables(i)%pool(k)%minimum_character = minimum_character
        if (allocated(tables(i)%pool(k)%contents)) deallocate (tables(i)%pool(k)%contents, tables(i)%pool(k)%sub)
        allocate (tables(i)%pool(k)%contents(chartab_size(depth)), tables(i)%pool(k)%sub(chartab_size(depth)))
        tables(i)%pool(k)%contents = defalt
        tables(i)%pool(k)%sub = 0
    end function make_sub_character_table

    !> Give sub char-table K of table I, and the deeper ones it holds,
    !> back to the pool's free list.  Not a port: upstream leaves them to
    !> GC.
    recursive subroutine destroy_sub(i, k)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k
        integer, allocatable :: grown(:)
        integer :: j

        do j = 1, size(tables(i)%pool(k)%sub)
            if (tables(i)%pool(k)%sub(j) > 0) call destroy_sub(i, tables(i)%pool(k)%sub(j))
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
    end subroutine destroy_sub

    !> Set element INDEX (0-based) of sub char-table K of table I (the top
    !> level when K is 0) to the value VALUE, releasing a sub char-table it
    !> held.  Not a port (set_character_table_contents and
    !> set_sub_character_table_contents with a value).
    recursive subroutine set_element(i, k, index, value)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k, index
        integer(int64), intent(in) :: value
        integer :: s

        if (k == 0) then
            s = tables(i)%sub(index)
            tables(i)%sub(index) = 0
            tables(i)%contents(index) = value
        else
            s = tables(i)%pool(k)%sub(index + 1)
            tables(i)%pool(k)%sub(index + 1) = 0
            tables(i)%pool(k)%contents(index + 1) = value
        end if

        if (s > 0) call destroy_sub(i, s)
    end subroutine set_element

    !> Element INDEX (0-based) of sub char-table K of table I (the top level
    !> when K is 0): its value VALUE, or the sub char-table S it holds
    !> (0 for a value).  Not a port.
    pure subroutine get_element(i, k, index, value, s)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k, index
        integer(int64), intent(out) :: value
        integer, intent(out) :: s

        if (k == 0) then
            value = tables(i)%contents(index)
            s = tables(i)%sub(index)
        else
            value = tables(i)%pool(k)%contents(index + 1)
            s = tables(i)%pool(k)%sub(index + 1)
        end if
    end subroutine get_element

    !> The index of element C of a sub char-table of DEPTH from MINIMUM_CHARACTER.
    !> Port of chartab.c CHARTAB_IDX.
    pure integer function chartab_idx(c, depth, minimum_character)
        integer, intent(in) :: c, depth, minimum_character

        chartab_idx = ishft(c - minimum_character, -chartab_bits(depth))
    end function chartab_idx

    !> Recompute the ASCII cache of table I: the sub char-table of depth 3
    !> holding the ASCII characters, or their common value.  Port of
    !> chartab.c:char_table_ascii and chartab.c:set_char_table_ascii.
    subroutine set_character_table_ascii(i)
        integer(int64), intent(in) :: i
        integer(int64) :: value
        integer :: s

        tables(i)%ascii_sub = 0
        call get_element(i, 0, 0, value, s)

        if (s > 0) then
            call get_element(i, s, 0, value, s)

            if (s > 0) then
                call get_element(i, s, 0, value, s)

                if (s > 0) then
                    tables(i)%ascii_sub = s
                    value = NO_VALUE
                end if
            end if
        end if

        tables(i)%ascii = value
    end subroutine set_character_table_ascii

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
    !> purpose name's `character_table_extra_slots' property).
    subroutine make_character_table(h, purpose, status, init, n_extras)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: purpose
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: init
        integer, intent(in), optional :: n_extras
        integer(int64) :: value
        integer :: n

        status = status_type()
        value = NO_VALUE
        if (present(init)) value = init
        n = purpose_extra_slots(purpose)
        if (present(n_extras)) n = n_extras

        if (n < 0 .or. n > MAXIMUM_EXTRA_SLOTS) then
            call set_error(status, ARGUMENTS_OUT_OF_RANGE_CODE, "character_table_extra_slots")
            h = null_handle()
            return
        end if

        call make_table(h)

        associate (t => tables(h%index))
            t%purpose = purpose
            t%defalt = value
            t%ascii = value
            t%contents = value
            allocate (t%extras(n), t%extra_tables(n))
            t%extras = value
        end associate
    end subroutine make_character_table

    !> Return a copy of char-table TABLE: its sub char-tables are copied,
    !> the values and the parent are shared.  Port of
    !> chartab.c:copy_char_table and chartab.c:copy_sub_char_table (assignment
    !> copies the pool).
    subroutine copy_character_table(table, copy, status)
        type(handle_type), intent(in) :: table
        type(handle_type), intent(out) :: copy
        type(status_type), intent(out) :: status
        integer(int64) :: i
        type(character_table_type) :: temporary

        call check_character_table(table, i, status)

        if (.not. status%ok()) then
            copy = null_handle()
            return
        end if

        temporary = tables(i)
        call make_table(copy)
        call move_alloc_table(temporary, tables(copy%index))
    end subroutine copy_character_table

    !> Move char-table FROM into TO.  Not a port.
    subroutine move_alloc_table(from, to)
        type(character_table_type), intent(inout) :: from
        type(character_table_type), intent(inout) :: to

        to = character_table_type()
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

    !> Return the subtype of char-table CHARACTER_TABLE.  The value is a name.
    !> Port of chartab.c:Fchar_table_subtype.
    function character_table_subtype(character_table, status) result(purpose)
        type(handle_type), intent(in) :: character_table
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: purpose
        integer(int64) :: i

        call check_character_table(character_table, i, status)

        if (.not. status%ok()) then
            purpose = ""
            return
        end if

        purpose = tables(i)%purpose
    end function character_table_subtype

    !> Return the parent char-table of CHARACTER_TABLE.
    !> The value is either the null handle or another char-table.
    !> If CHARACTER_TABLE holds `NO_VALUE' for a given character,
    !> then the actual applicable value is inherited from the parent char-table
    !> (or from its parents, if necessary).  Port of
    !> chartab.c:Fchar_table_parent.
    function character_table_parent(character_table, status) result(parent)
        type(handle_type), intent(in) :: character_table
        type(status_type), intent(out) :: status
        type(handle_type) :: parent
        integer(int64) :: i

        parent = null_handle()
        call check_character_table(character_table, i, status)
        if (.not. status%ok()) return
        if (registry%is_live(tables(i)%parent)) parent = tables(i)%parent
    end function character_table_parent

    !> Set the parent char-table of CHARACTER_TABLE to PARENT.
    !> PARENT must be either the null handle or another char-table.
    !> Port of chartab.c:Fset_char_table_parent and chartab.c:set_char_table_parent.
    subroutine set_character_table_parent(character_table, parent, status)
        type(handle_type), intent(in) :: character_table, parent
        type(status_type), intent(out) :: status
        integer(int64) :: i, j
        type(handle_type) :: temporary

        call check_character_table(character_table, i, status)
        if (.not. status%ok()) return

        if (parent%index /= 0) then
            call check_character_table(parent, j, status)
            if (.not. status%ok()) return
            temporary = parent

            do while (registry%is_live(temporary))
                if (temporary%index == character_table%index .and. temporary%generation == character_table%generation) then
                    call set_error(status, GENERIC_ERROR_CODE, "Attempt to make a chartable be its own parent")
                    return
                end if

                temporary = tables(temporary%index)%parent
            end do
        end if

        tables(i)%parent = parent
    end subroutine set_character_table_parent

    !> Return the value of CHARACTER_TABLE's extra-slot number N.  Port of
    !> chartab.c:Fchar_table_extra_slot (N counts from 0).
    integer(int64) function character_table_extra_slot(character_table, n, status) result(value)
        type(handle_type), intent(in) :: character_table
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        integer(int64) :: i

        value = NO_VALUE
        call check_character_table(character_table, i, status)
        if (.not. status%ok()) return

        if (n < 0 .or. n >= size(tables(i)%extras)) then
            call set_error(status, ARGUMENTS_OUT_OF_RANGE_CODE, "character_table_extra_slot")
            return
        end if

        value = tables(i)%extras(n + 1)
    end function character_table_extra_slot

    !> Set CHARACTER_TABLE's extra-slot number N to VALUE.  Port of
    !> chartab.c:Fset_char_table_extra_slot (N counts from 0).
    subroutine set_character_table_extra_slot(character_table, n, value, status)
        type(handle_type), intent(in) :: character_table
        integer, intent(in) :: n
        integer(int64), intent(in) :: value
        type(status_type), intent(out) :: status
        integer(int64) :: i

        call check_character_table(character_table, i, status)
        if (.not. status%ok()) return

        if (n < 0 .or. n >= size(tables(i)%extras)) then
            call set_error(status, ARGUMENTS_OUT_OF_RANGE_CODE, "set_character_table_extra_slot")
            return
        end if

        tables(i)%extras(n + 1) = value
        tables(i)%extra_tables(n + 1) = null_handle()
    end subroutine set_character_table_extra_slot

    !> Return the char-table in CHARACTER_TABLE's extra-slot number N, or the
    !> null handle when it holds none.  Port of
    !> chartab.c:Fchar_table_extra_slot for a slot holding a char-table.
    function character_table_extra_table(character_table, n, status) result(value)
        type(handle_type), intent(in) :: character_table
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        type(handle_type) :: value
        integer(int64) :: i

        value = null_handle()
        call check_character_table(character_table, i, status)
        if (.not. status%ok()) return

        if (n < 0 .or. n >= size(tables(i)%extras)) then
            call set_error(status, ARGUMENTS_OUT_OF_RANGE_CODE, "character_table_extra_slot")
            return
        end if

        value = tables(i)%extra_tables(n + 1)
    end function character_table_extra_table

    !> Set CHARACTER_TABLE's extra-slot number N to the char-table VALUE (the
    !> null handle to empty it).  Port of
    !> chartab.c:Fset_char_table_extra_slot for a char-table value.
    subroutine set_character_table_extra_table(character_table, n, value, status)
        type(handle_type), intent(in) :: character_table
        integer, intent(in) :: n
        type(handle_type), intent(in) :: value
        type(status_type), intent(out) :: status
        integer(int64) :: i

        call check_character_table(character_table, i, status)
        if (.not. status%ok()) return

        if (n < 0 .or. n >= size(tables(i)%extras)) then
            call set_error(status, ARGUMENTS_OUT_OF_RANGE_CODE, "set_character_table_extra_slot")
            return
        end if

        tables(i)%extras(n + 1) = NO_VALUE
        tables(i)%extra_tables(n + 1) = value
    end subroutine set_character_table_extra_table

    ! ------------------------------------------------------------------
    ! Lookup
    ! ------------------------------------------------------------------

    !> The value for C in sub char-table K of table I, without the default
    !> value or the parent.  Port of chartab.c:sub_char_table_ref.
    pure recursive integer(int64) function sub_character_table_reference(i, k, c) result(value)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k, c
        integer :: s

        call get_element(i, k, chartab_idx(c, tables(i)%pool(k)%depth, tables(i)%pool(k)%minimum_character), value, s)
        if (s > 0) value = sub_character_table_reference(i, s, c)
    end function sub_character_table_reference

    !> The value for C in table I, without the default value or the
    !> parent.  Not a port (character_table_reference's first part).
    pure integer(int64) function own_reference(i, c) result(value)
        integer(int64), intent(in) :: i
        integer, intent(in) :: c
        integer :: s

        if (c < 128) then
            value = tables(i)%ascii
            if (tables(i)%ascii_sub > 0) value = tables(i)%pool(tables(i)%ascii_sub)%contents(c + 1)
        else
            call get_element(i, 0, chartab_idx(c, 0, 0), value, s)
            if (s > 0) value = sub_character_table_reference(i, s, c)
        end if
    end function own_reference

    !> The value for character C in char-table TABLE: its own, else the
    !> default value, else the parent's; `NO_VALUE' if none.  An invalid
    !> TABLE has no values.  Port of chartab.c:char_table_ref
    !> (CHARACTER_TABLE_REFERENCE).
    pure recursive integer(int64) function character_table_reference(table, c) result(value)
        type(handle_type), intent(in) :: table
        integer, intent(in) :: c
        integer(int64) :: i
        type(status_type) :: status

        value = NO_VALUE
        call check_character_table(table, i, status)
        if (.not. status%ok()) return
        if (c < 0 .or. c > MAXIMUM_CHARACTER) return
        value = own_reference(i, c)

        if (value == NO_VALUE) then
            value = tables(i)%defalt
            if (value == NO_VALUE .and. registry%is_live(tables(i)%parent)) value = character_table_reference(tables(i)%parent, c)
        end if
    end function character_table_reference

    !> Element INDEX of sub char-table K of table I (the top level when K is
    !> 0) for C, with DEFALT for `NO_VALUE', shrinking FROM..TO as for
    !> `character_table_reference_and_range'.  Port of chartab.c:char_table_ref_simple.
    recursive integer(int64) function character_table_reference_simple(i, k, index, c, from, to, defalt) result(value)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k, index, c
        integer, intent(inout) :: from, to
        integer(int64), intent(in) :: defalt
        integer :: s

        call get_element(i, k, index, value, s)

        if (s > 0) then
            value = sub_character_table_reference_and_range(i, s, c, from, to, defalt)
        else if (value == NO_VALUE) then
            value = defalt
        end if
    end function character_table_reference_simple

    !> Port of chartab.c:sub_char_table_ref_and_range.
    recursive integer(int64) function sub_character_table_reference_and_range(i, k, c, from, to, defalt) result(value)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k, c
        integer, intent(inout) :: from, to
        integer(int64), intent(in) :: defalt
        integer :: depth, minimum_character, index, cidx, cc
        integer(int64) :: this_value

        depth = tables(i)%pool(k)%depth
        minimum_character = tables(i)%pool(k)%minimum_character
        cidx = chartab_idx(c, depth, minimum_character)
        value = character_table_reference_simple(i, k, cidx, c, from, to, defalt)
        index = cidx

        do while (index > 0 .and. from < minimum_character + index*chartab_characters(depth))
            cc = minimum_character + index*chartab_characters(depth) - 1
            index = index - 1
            this_value = character_table_reference_simple(i, k, index, cc, from, to, defalt)

            if (this_value /= value) then
                from = cc + 1
                exit
            end if
        end do

        do
            cc = (cidx + 1)*chartab_characters(depth)
            if (cc >= chartab_characters(depth - 1)) exit
            cc = cc + minimum_character
            if (cc > to) exit
            cidx = cidx + 1
            this_value = character_table_reference_simple(i, k, cidx, cc, from, to, defalt)

            if (this_value /= value) then
                to = cc - 1
                exit
            end if
        end do
    end function sub_character_table_reference_and_range

    !> Return the value for C in char-table TABLE.  Shrink the range FROM
    !> and TO to cover characters (containing C) that have the same value
    !> as C.  It is not assured that the values of (FROM - 1) and (TO +
    !> 1) are different from that of C.  A negative FROM is 0, a negative
    !> TO `MAXIMUM_CHARACTER'.  The parent is not consulted.  Port of
    !> chartab.c:char_table_ref_and_range.
    integer(int64) function character_table_reference_and_range(table, c, from, to) result(value)
        type(handle_type), intent(in) :: table
        integer, intent(in) :: c
        integer, intent(inout) :: from, to
        integer(int64) :: i, this_value, defalt
        integer :: cidx, index, cc
        type(status_type) :: status

        value = NO_VALUE
        call check_character_table(table, i, status)
        if (.not. status%ok()) return
        if (from < 0) from = 0
        if (to < 0) to = MAXIMUM_CHARACTER
        defalt = tables(i)%defalt
        cidx = chartab_idx(c, 0, 0)
        value = character_table_reference_simple(i, 0, cidx, c, from, to, defalt)
        index = cidx

        do while (from < index*chartab_characters(0))
            cc = index*chartab_characters(0) - 1
            index = index - 1
            this_value = character_table_reference_simple(i, 0, index, cc, from, to, defalt)

            if (this_value /= value) then
                from = cc + 1
                exit
            end if
        end do

        do while (to >= (cidx + 1)*chartab_characters(0))
            cidx = cidx + 1
            cc = cidx*chartab_characters(0)
            this_value = character_table_reference_simple(i, 0, cidx, cc, from, to, defalt)

            if (this_value /= value) then
                to = cc - 1
                exit
            end if
        end do
    end function character_table_reference_and_range

    ! ------------------------------------------------------------------
    ! Setting
    ! ------------------------------------------------------------------

    !> Port of chartab.c:sub_char_table_set.
    recursive subroutine sub_character_table_set(i, k, c, value)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k, c
        integer(int64), intent(in) :: value
        integer :: depth, minimum_character, index, s
        integer(int64) :: old

        depth = tables(i)%pool(k)%depth
        minimum_character = tables(i)%pool(k)%minimum_character
        index = chartab_idx(c, depth, minimum_character)

        if (depth == 3) then
            call set_element(i, k, index, value)
        else
            call get_element(i, k, index, old, s)

            if (s == 0) then
                s = make_sub_character_table(i, depth + 1, minimum_character + index*chartab_characters(depth), old)
                tables(i)%pool(k)%sub(index + 1) = s
            end if

            call sub_character_table_set(i, s, c, value)
        end if
    end subroutine sub_character_table_set

    !> Set the value for character C in char-table TABLE to VALUE.  Port of
    !> chartab.c:char_table_set (CHARACTER_TABLE_SET).
    subroutine character_table_set(table, c, value, status)
        type(handle_type), intent(in) :: table
        integer, intent(in) :: c
        integer(int64), intent(in) :: value
        type(status_type), intent(out) :: status
        integer(int64) :: i, old
        integer :: index, s

        call check_character_table(table, i, status)
        if (.not. status%ok()) return

        if (c < 0 .or. c > MAXIMUM_CHARACTER) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected integer (a character code)")
            return
        end if

        if (c < 128 .and. tables(i)%ascii_sub > 0) then
            call set_element(i, tables(i)%ascii_sub, c, value)
        else
            index = chartab_idx(c, 0, 0)
            call get_element(i, 0, index, old, s)

            if (s == 0) then
                s = make_sub_character_table(i, 1, index*chartab_characters(0), old)
                tables(i)%sub(index) = s
            end if

            call sub_character_table_set(i, s, c, value)
            if (c < 128) call set_character_table_ascii(i)
        end if
    end subroutine character_table_set

    !> Port of chartab.c:sub_char_table_set_range.
    recursive subroutine sub_character_table_set_range(i, k, from, to, value)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k, from, to
        integer(int64), intent(in) :: value
        integer :: depth, minimum_character, characters_in_block, index, c, limit, s, start
        integer(int64) :: old

        depth = tables(i)%pool(k)%depth
        minimum_character = tables(i)%pool(k)%minimum_character
        characters_in_block = chartab_characters(depth)
        limit = chartab_size(depth)
        start = max(from, minimum_character)
        index = chartab_idx(start, depth, minimum_character)
        c = minimum_character + characters_in_block*index

        do while (index < limit)
            if (c > to) exit

            if (start <= c .and. c + characters_in_block - 1 <= to) then
                call set_element(i, k, index, value)
            else
                call get_element(i, k, index, old, s)

                if (s == 0) then
                    s = make_sub_character_table(i, depth + 1, c, old)
                    tables(i)%pool(k)%sub(index + 1) = s
                end if

                call sub_character_table_set_range(i, s, start, to, value)
            end if

            index = index + 1
            c = c + characters_in_block
        end do
    end subroutine sub_character_table_set_range

    !> Set the value for the characters FROM to TO in char-table TABLE to
    !> VALUE.  A reversed range sets nothing.  Port of
    !> chartab.c:char_table_set_range.
    subroutine character_table_set_range(table, from, to, value, status)
        type(handle_type), intent(in) :: table
        integer, intent(in) :: from, to
        integer(int64), intent(in) :: value
        type(status_type), intent(out) :: status
        integer(int64) :: i, old
        integer :: index, limit, c, s

        if (from == to) then
            call character_table_set(table, from, value, status)
            return
        end if

        call check_character_table(table, i, status)
        if (.not. status%ok()) return

        if (from < 0 .or. from > MAXIMUM_CHARACTER .or. to < 0 .or. to > MAXIMUM_CHARACTER) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected integer (a character code)")
            return
        end if

        limit = chartab_idx(to, 0, 0)
        index = chartab_idx(from, 0, 0)
        c = index*chartab_characters(0)

        do while (index <= limit)
            if (c > to) exit

            if (from <= c .and. c + chartab_characters(0) - 1 <= to) then
                call set_element(i, 0, index, value)
            else
                call get_element(i, 0, index, old, s)

                if (s == 0) then
                    s = make_sub_character_table(i, 1, index*chartab_characters(0), old)
                    tables(i)%sub(index) = s
                end if

                call sub_character_table_set_range(i, s, from, to, value)
            end if

            index = index + 1
            c = c + chartab_characters(0)
        end do

        if (from < 128) call set_character_table_ascii(i)
    end subroutine character_table_set_range

    !> Return the value in CHARACTER_TABLE for a range of characters RANGE.
    !> RANGE should be absent (for the default value),
    !> FROM and TO (for characters in the range), or FROM alone (a character code).
    !> If RANGE is FROM..TO, the function returns the value for FROM.
    !> Port of chartab.c:Fchar_table_range.
    integer(int64) function character_table_range(character_table, status, from, to) result(value)
        type(handle_type), intent(in) :: character_table
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: from, to
        integer(int64) :: i
        integer :: f, t

        value = NO_VALUE
        call check_character_table(character_table, i, status)
        if (.not. status%ok()) return

        if (.not. present(from)) then
            value = tables(i)%defalt
            return
        end if

        if (from < 0 .or. from > MAXIMUM_CHARACTER) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected integer (a character code)")
            return
        end if

        if (.not. present(to)) then
            value = character_table_reference(character_table, from)
            return
        end if

        if (to < 0 .or. to > MAXIMUM_CHARACTER) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected integer (a character code)")
            return
        end if

        f = from
        t = to
        value = character_table_reference_and_range(character_table, from, f, t)
    end function character_table_range

    !> Set the value in CHARACTER_TABLE for a range of characters RANGE to VALUE.
    !> RANGE should be ALL true (for all characters), absent (for the default value),
    !> FROM and TO (for characters in the range),
    !> or FROM alone (a character code).  Port of chartab.c:Fset_char_table_range.
    subroutine set_character_table_range(character_table, value, status, from, to, all)
        type(handle_type), intent(in) :: character_table
        integer(int64), intent(in) :: value
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: from, to
        logical, intent(in), optional :: all
        integer(int64) :: i
        integer :: j
        logical :: every

        call check_character_table(character_table, i, status)
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
            call character_table_set(character_table, from, value, status)
        else
            call character_table_set_range(character_table, from, to, value, status)
        end if
    end subroutine set_character_table_range

    ! ------------------------------------------------------------------
    ! Optimizing
    ! ------------------------------------------------------------------

    !> Optimize sub char-table K of table I: when all its elements are the
    !> same value, return true with that value in ELEMENT.  Port of
    !> chartab.c:optimize_sub_char_table (values compare by ==).
    recursive logical function optimize_sub_character_table(i, k, element) result(optimizable)
        integer(int64), intent(in) :: i
        integer, intent(in) :: k
        integer(int64), intent(out) :: element
        integer(int64) :: this
        integer :: j, s

        optimizable = .true.

        do j = 0, chartab_size(tables(i)%pool(k)%depth) - 1
            call get_element(i, k, j, this, s)

            if (s > 0) then
                if (optimize_sub_character_table(i, s, this)) call set_element(i, k, j, this)
                call get_element(i, k, j, this, s)
            end if

            if (j == 0) then
                element = this
                optimizable = s == 0
            else if (optimizable) then
                optimizable = s == 0
                if (optimizable) optimizable = this == element
            end if
        end do
    end function optimize_sub_character_table

    !> Optimize CHARACTER_TABLE: merge sub char-tables whose entries are all
    !> equal.  Port of chartab.c:Foptimize_char_table (no TEST: the values
    !> are integers, equal when ==).
    subroutine optimize_character_table(character_table, status)
        type(handle_type), intent(in) :: character_table
        type(status_type), intent(out) :: status
        integer(int64) :: i, element, this
        integer :: j, s

        call check_character_table(character_table, i, status)
        if (.not. status%ok()) return

        do j = 0, chartab_size(0) - 1
            call get_element(i, 0, j, this, s)

            if (s > 0) then
                if (optimize_sub_character_table(i, s, element)) call set_element(i, 0, j, element)
            end if
        end do

        ! Reset the `ascii' cache, in case it got optimized away.
        call set_character_table_ascii(i)
    end subroutine optimize_character_table

    ! ------------------------------------------------------------------
    ! Mapping
    ! ------------------------------------------------------------------

    !> Map PROCESS over sub char-table K of table I (the top level when K is
    !> 0), calling it for each character or group of characters that
    !> share a value.  RFROM..RTO is the range of target characters, VALUE
    !> the value of RFROM in the table.
    !>
    !> VALUE becomes the value of the last character covered by the table
    !> (not the value inherited from the parent), and RFROM the minimum
    !> character C where C and all the following characters in the table
    !> have the same value.  Port of chartab.c:map_sub_char_table.
    recursive subroutine map_sub_character_table(operation, argument, i, k, value, rfrom, rto)
        procedure(map_character_table_procedure) :: operation
        type(handle_type), intent(in) :: argument
        integer(int64), intent(in) :: i
        integer, intent(in) :: k
        integer(int64), intent(inout) :: value
        integer, intent(inout) :: rfrom, rto
        integer :: depth, minimum_character, last_character, characters_in_block, from, to, index, c, nextc, s
        integer(int64) :: this, p
        logical :: different_value
        type(status_type) :: status

        from = rfrom
        to = rto

        if (k > 0) then
            depth = tables(i)%pool(k)%depth
            minimum_character = tables(i)%pool(k)%minimum_character
            last_character = minimum_character + chartab_characters(depth - 1) - 1
        else
            depth = 0
            minimum_character = 0
            last_character = MAXIMUM_CHARACTER
        end if

        characters_in_block = chartab_characters(depth)
        if (to < last_character) last_character = to
        ! Set INDEX to the index of the first element to check.
        index = 0
        if (from > minimum_character) index = (from - minimum_character)/characters_in_block
        c = minimum_character + characters_in_block*index

        do while (c <= last_character)
            call get_element(i, k, index, this, s)
            nextc = c + characters_in_block

            if (s > 0) then
                if (to >= nextc) rto = nextc - 1
                call map_sub_character_table(operation, argument, i, s, value, rfrom, rto)
            else
                if (this == NO_VALUE) this = tables(i)%defalt

                if (value /= this) then
                    different_value = .true.

                    if (value == NO_VALUE .and. registry%is_live(tables(i)%parent)) then
                        call check_character_table(tables(i)%parent, p, status)
                        ! The value of FROM in the parent without checking
                        ! the parent of the parent.
                        value = own_reference(p, from)
                        if (value == NO_VALUE) value = tables(p)%defalt
                        rto = c - 1
                        call map_sub_character_table(operation, argument, p, 0, value, rfrom, rto)
                        if (value == this) different_value = .false.
                    end if

                    if (value /= NO_VALUE .and. different_value) then
                        rto = c - 1
                        call operation(argument, rfrom, rto, value)
                    end if

                    value = this
                    from = c
                    rfrom = c
                end if
            end if

            rto = to
            index = index + 1
            c = c + characters_in_block
        end do
    end subroutine map_sub_character_table

    !> Call OPERATION for each character in TABLE that has a value: with ARGUMENT,
    !> the characters FROM..TO (FROM = TO for one character) of a run that
    !> shares a value, and that value (what `character_table_range' returns for
    !> the run).  Runs follow each other in character order, and where
    !> TABLE has no value its parents are consulted.  Port of
    !> chartab.c:map_char_table, which `Fmap_char_table' calls with ARGUMENT
    !> TABLE (chartab.c:Fmap_char_table).
    recursive subroutine map_character_table(operation, table, status, argument)
        procedure(map_character_table_procedure) :: operation
        type(handle_type), intent(in) :: table
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: argument
        type(handle_type) :: a, parent
        integer(int64) :: i, value
        integer :: rfrom, rto

        call check_character_table(table, i, status)
        if (.not. status%ok()) return
        a = table
        if (present(argument)) a = argument
        rfrom = 0
        rto = MAXIMUM_CHARACTER
        value = tables(i)%ascii
        if (tables(i)%ascii_sub > 0) value = tables(i)%pool(tables(i)%ascii_sub)%contents(1)
        call map_sub_character_table(operation, a, i, 0, value, rfrom, rto)

        ! If VALUE is unset and TABLE has a parent, we must consult the parent
        ! recursively.
        do while (value == NO_VALUE .and. registry%is_live(tables(i)%parent))
            parent = tables(i)%parent
            call check_character_table(parent, i, status)
            ! The value of FROM in the parent without checking the parent
            ! of the parent.
            value = own_reference(i, rfrom)
            if (value == NO_VALUE) value = tables(i)%defalt
            call map_sub_character_table(operation, a, i, 0, value, rfrom, rto)
        end do

        status = status_type()
        if (value /= NO_VALUE) call operation(a, rfrom, rto, value)
    end subroutine map_character_table
end module character_table_mod
