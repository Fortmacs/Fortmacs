! category_fm.f90 -- categories, category sets and category tables.
! Ported from GNU Emacs 31.1 src/category.c, src/category.h.
!
! Copyright (C) 1998, 2001-2026 Free Software Foundation, Inc.
! Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
!   2005, 2006, 2007, 2008, 2009, 2010, 2011
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H14PRO021
! Copyright (C) 2003
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

!> Categories (category.h).  A category is like syntax but differs in
!> the following points:
!>  - A category is represented by a mnemonic character of the range
!>    ` ' (32) to `~' (126) (printable ASCII characters).
!>  - A category is not exclusive, i.e. a character has multiple
!>    categories (category set).  Of course, there's a case that a
!>    category set is empty, i.e. the character has no category.
!>  - In addition to the predefined categories, a user can define new
!>    categories.  Total number of categories is limited to 95.
!>
!> A category set is a `category_set_type' (a bool-vector of length 128
!> upstream).  A category table is a char-table (chartab_fm) of subtype
!> "category_table" whose values are category set numbers; its extra
!> slot 0 holds the number of its `category_info_type', which keeps the
!> docstrings of its categories (upstream's extra slot 0) and the
!> distinct category sets it holds (upstream's hash table in extra slot
!> 1, which makes equal sets one object).  Each buffer has a category
!> table, the standard one unless `set_category_table' gave it another
!> (BVAR category_table in buffer_fm, a null handle for the standard
!> one).  The standard table, made on first use, has the categories
!> characters.el defines that are ported, with their characters
!> (characters_fm).
module category_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_WRONG_TYPE_ARGUMENT
    use handle_fm, only: handle_type, null_handle
    use buffer_fm, only: buffers, cur
    use character_fm, only: MAX_CHAR
    use chartab_fm, only: make_char_table, copy_char_table, char_table_p, char_table_subtype, &
                          char_table_extra_slot, set_char_table_extra_slot, char_table_ref, char_table_ref_and_range, &
                          char_table_set_range, set_char_table_range, NO_VALUE
    use characters_fm, only: characters_categories
    implicit none(type, external)
    private

    public :: category_set_type, make_category_set, define_category, category_docstring, get_unused_category, &
              category_table_p, category_table, standard_category_table, copy_category_table, make_category_table, &
              set_category_table, char_category_set, category_set_mnemonics, modify_category_entry, &
              char_has_category, category_member

    !> A category set: `member(c)' is true when category `c' (32 to 126)
    !> is in the set.  Port of category.h's category sets (bool-vectors
    !> of length 128).
    type :: category_set_type
        logical :: member(0:127) = .false.
    end type category_set_type

    !> The docstring of one category; unallocated while the category is
    !> undefined.
    type :: category_docstring_type
        character(len=:), allocatable :: text
    end type category_docstring_type

    !> What a category table keeps besides its char-table: the docstrings
    !> of its categories (extra slot 0 upstream) and its distinct category
    !> sets, numbered from 1 (the hash table in extra slot 1).
    type :: category_info_type
        type(category_docstring_type) :: docstrings(32:126)
        type(category_set_type), allocatable :: sets(:)
        integer :: nsets = 0
    end type category_info_type

    type(category_info_type), allocatable :: infos(:)
    integer :: ninfos = 0

    !> The standard category table (category.h Vstandard_category_table);
    !> null until `init_category_once' runs.
    type(handle_type) :: standard

contains

    ! ------------------------------------------------------------------
    ! Category sets
    ! ------------------------------------------------------------------

    !> Whether C is a category: a character from ` ' to `~'.  Port of
    !> category.h CATEGORYP.
    pure logical function categoryp(c)
        character, intent(in) :: c

        categoryp = iachar(c) >= 32 .and. iachar(c) <= 126
    end function categoryp

    !> True if CATEGORY_SET contains CATEGORY.  Port of
    !> category.h:CATEGORY_MEMBER.
    pure logical function category_member(category, category_set)
        character, intent(in) :: category
        type(category_set_type), intent(in) :: category_set

        category_member = category_set%member(iachar(category))
    end function category_member

    !> Make CATEGORY_SET include (if VAL) or exclude (if not VAL)
    !> CATEGORY.  Port of category.c:set_category_set.
    pure subroutine set_category_set(category_set, category, val)
        type(category_set_type), intent(inout) :: category_set
        character, intent(in) :: category
        logical, intent(in) :: val

        category_set%member(iachar(category)) = val
    end subroutine set_category_set

    !> Return a newly created category-set which contains CATEGORIES.
    !> CATEGORIES is a string of category mnemonics.
    !> The value is a category set which has `member' true at the indices
    !> corresponding to those categories.  Port of
    !> category.c:Fmake_category_set.
    subroutine make_category_set(categories, val, status)
        character(len=*), intent(in) :: categories
        type(category_set_type), intent(out) :: val
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()

        if (any([(iachar(categories(k:k)) > 127, k=1, len(categories))])) then
            call set_error(status, FM_GENERIC_ERROR, "Multibyte string in `make_category_set'")
            return
        end if

        do k = len(categories), 1, -1
            if (.not. categoryp(categories(k:k))) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected category character(len=1)")
                return
            end if

            call set_category_set(val, categories(k:k), .true.)
        end do
    end subroutine make_category_set

    !> Return a string containing mnemonics of the categories in CATEGORY_SET.
    !> CATEGORY_SET is a category set, and the categories "in" it are those
    !> whose `member' is true.
    !> The return value is a string containing those same categories.
    !> Port of category.c:Fcategory_set_mnemonics.
    function category_set_mnemonics(category_set) result(str)
        type(category_set_type), intent(in) :: category_set
        character(len=:), allocatable :: str
        integer :: i

        str = ""

        do i = 32, 126
            if (category_set%member(i)) str = str//achar(i)
        end do
    end function category_set_mnemonics

    ! ------------------------------------------------------------------
    ! Category tables
    ! ------------------------------------------------------------------

    !> Return true if ARG is a category table.  Port of
    !> category.c:Fcategory_table_p.
    logical function category_table_p(arg)
        type(handle_type), intent(in) :: arg
        type(status_type) :: status

        category_table_p = .false.
        if (char_table_p(arg)) category_table_p = char_table_subtype(arg, status) == "category_table"
    end function category_table_p

    !> If TABLE is absent, give the current category table.  If TABLE is
    !> present, check the validity of TABLE as a category table: give
    !> TABLE itself if valid, else a
    !> `wrong_type_argument' error condition occurs.  INFO is the number of its info.  Port of
    !> category.c:check_category_table.
    recursive subroutine check_category_table(h, info, status, table)
        type(handle_type), intent(out) :: h
        integer, intent(out) :: info
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: table
        type(status_type) :: st

        info = 0
        status = status_type()

        if (present(table)) then
            h = table

            if (.not. category_table_p(h)) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected category table type(char_table_type)")
                return
            end if
        else
            h = category_table()
        end if

        info = int(char_table_extra_slot(h, 0, st))
    end subroutine check_category_table

    !> Return the current category table.
    !> This is the one specified by the current buffer.  Port of
    !> category.c:Fcategory_table.
    recursive function category_table() result(h)
        type(handle_type) :: h

        h = standard_category_table()
        if (cur == 0) return
        if (category_table_p(buffers(cur)%category_table)) h = buffers(cur)%category_table
    end function category_table

    !> Return the standard category table.
    !> This is the one used for new buffers.  Port of
    !> category.c:Fstandard_category_table (made on first use).
    recursive function standard_category_table() result(h)
        type(handle_type) :: h

        if (.not. category_table_p(standard)) call init_category_once()
        h = standard
    end function standard_category_table

    !> A new info with no category defined and no set but the empty one;
    !> its number.  Not a port.
    integer function new_info() result(n)
        type(category_info_type), allocatable :: grown(:)

        if (.not. allocated(infos)) then
            allocate (infos(8))
        else if (ninfos == size(infos)) then
            allocate (grown(2*ninfos))
            grown(1:ninfos) = infos
            call move_alloc(grown, infos)
        end if

        ninfos = ninfos + 1
        n = ninfos
        allocate (infos(n)%sets(8))
        infos(n)%nsets = 1
    end function new_info

    !> A new char-table for a category table, with a new info in its extra
    !> slot 0 and the empty set as its default value.  Not a port (the
    !> part of Fmake_category_table and init_category_once they share).
    subroutine new_category_char_table(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call make_char_table(h, "category_table", st)
        ! Set a category set which contains nothing to the default.
        call set_char_table_range(h, 1_int64, st)
        call set_char_table_extra_slot(h, 0, int(new_info(), int64), st)
    end subroutine new_category_char_table

    !> Construct a new and empty category table and return it.  Port of
    !> category.c:Fmake_category_table.
    function make_category_table() result(h)
        type(handle_type) :: h
        type(status_type) :: st

        call new_category_char_table(h)
        call set_char_table_range(h, 1_int64, st, 0, MAX_CHAR)
    end function make_category_table

    !> Construct a new category table and return it.
    !> It is a copy of the TABLE, which defaults to the standard category table.
    !> Port of category.c:Fcopy_category_table, category.c:copy_category_table
    !> and category.c:copy_category_entry (the category sets are copied
    !> with the table's info).
    subroutine copy_category_table(copy, status, table)
        type(handle_type), intent(out) :: copy
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: table
        type(handle_type) :: h
        type(status_type) :: st
        integer :: info, n

        copy = null_handle()

        if (present(table)) then
            call check_category_table(h, info, status, table)
            if (.not. status%ok()) return
        else
            h = standard_category_table()
            info = int(char_table_extra_slot(h, 0, st))
        end if

        call copy_char_table(h, copy, st)
        n = new_info()
        infos(n) = infos(info)
        call set_char_table_extra_slot(copy, 0, int(n, int64), st)
    end subroutine copy_category_table

    !> Specify TABLE as the category table for the current buffer.
    !> Port of category.c:Fset_category_table and category.c:bset_category_table.
    subroutine set_category_table(table, status)
        type(handle_type), intent(in) :: table
        type(status_type), intent(out) :: status
        type(handle_type) :: h
        integer :: info

        call check_category_table(h, info, status, table)
        if (.not. status%ok()) return
        if (cur /= 0) buffers(cur)%category_table = h
    end subroutine set_category_table

    !> The category set number CATEGORY_SET stands for in info INFO,
    !> adding it if no equal set is there yet.  Port of
    !> category.c:hash_get_category_set.
    integer function hash_get_category_set(info, category_set) result(n)
        integer, intent(in) :: info
        type(category_set_type), intent(in) :: category_set
        type(category_set_type), allocatable :: grown(:)

        do n = 1, infos(info)%nsets
            if (all(infos(info)%sets(n)%member .eqv. category_set%member)) return
        end do

        if (infos(info)%nsets == size(infos(info)%sets)) then
            allocate (grown(2*infos(info)%nsets))
            grown(1:infos(info)%nsets) = infos(info)%sets
            call move_alloc(grown, infos(info)%sets)
        end if

        infos(info)%nsets = infos(info)%nsets + 1
        n = infos(info)%nsets
        infos(info)%sets(n) = category_set
    end function hash_get_category_set

    ! ------------------------------------------------------------------
    ! Categories
    ! ------------------------------------------------------------------

    !> Define CATEGORY as a category which is described by DOCSTRING.
    !> CATEGORY should be an ASCII printing character in the range ` ' to `~'.
    !> DOCSTRING is the documentation string of the category.  The first line
    !> should be a terse text (preferably less than 16 characters),
    !> and the rest lines should be the full description.
    !> The category is defined only in category table TABLE, which defaults to
    !> the current buffer's category table.  Port of
    !> category.c:Fdefine_category.
    recursive subroutine define_category(category, docstring, status, table)
        character, intent(in) :: category
        character(len=*), intent(in) :: docstring
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: table
        type(handle_type) :: h
        integer :: info

        if (.not. categoryp(category)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected category character(len=1)")
            return
        end if

        call check_category_table(h, info, status, table)
        if (.not. status%ok()) return

        if (allocated(infos(info)%docstrings(iachar(category))%text)) then
            call set_error(status, FM_GENERIC_ERROR, "Category `"//category//"' is already defined")
            return
        end if

        infos(info)%docstrings(iachar(category))%text = docstring
    end subroutine define_category

    !> Return the documentation string of CATEGORY, as defined in TABLE.
    !> TABLE should be a category table and defaults to the current buffer's
    !> category table.  DEFINED is false (and the docstring empty) for an
    !> undefined category.  Port of category.c:Fcategory_docstring.
    function category_docstring(category, status, table, defined) result(doc)
        character, intent(in) :: category
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: table
        logical, intent(out), optional :: defined
        character(len=:), allocatable :: doc
        type(handle_type) :: h
        integer :: info

        doc = ""
        if (present(defined)) defined = .false.

        if (.not. categoryp(category)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected category character(len=1)")
            return
        end if

        call check_category_table(h, info, status, table)
        if (.not. status%ok()) return
        if (.not. allocated(infos(info)%docstrings(iachar(category))%text)) return
        doc = infos(info)%docstrings(iachar(category))%text
        if (present(defined)) defined = .true.
    end function category_docstring

    !> Return a category which is not yet defined in TABLE.
    !> If no category remains available, return `achar(0)'.
    !> The optional argument TABLE specifies which category
    !> table to modify; it defaults to the current buffer's category
    !> table.  Port of category.c:Fget_unused_category.
    function get_unused_category(status, table) result(category)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: table
        character :: category
        type(handle_type) :: h
        integer :: info, i

        category = achar(0)
        call check_category_table(h, info, status, table)
        if (.not. status%ok()) return

        do i = 32, 126
            if (.not. allocated(infos(info)%docstrings(i)%text)) then
                category = achar(i)
                return
            end if
        end do
    end function get_unused_category

    !> The category set of character C in category table H with info
    !> INFO.  Not a port.
    function table_category_set(h, info, c) result(category_set)
        type(handle_type), intent(in) :: h
        integer, intent(in) :: info, c
        type(category_set_type) :: category_set
        integer(int64) :: n

        n = char_table_ref(h, c)
        if (n >= 1 .and. n <= infos(info)%nsets) category_set = infos(info)%sets(n)
    end function table_category_set

    !> Return the category set of CHAR.  Port of
    !> category.c:Fchar_category_set and category.c:char_category_set
    !> (category.h CATEGORY_SET).
    recursive function char_category_set(ch) result(category_set)
        integer, intent(in) :: ch
        type(category_set_type) :: category_set
        type(handle_type) :: h
        type(status_type) :: st
        integer :: info

        call check_category_table(h, info, st)
        category_set = table_category_set(h, info, ch)
    end function char_category_set

    !> Return true if category set of CH contains CATEGORY.  Port of
    !> category.h:CHAR_HAS_CATEGORY.
    recursive logical function char_has_category(ch, category)
        integer, intent(in) :: ch
        character, intent(in) :: category
        type(category_set_type) :: category_set

        category_set = char_category_set(ch)
        char_has_category = category_member(category, category_set)
    end function char_has_category

    !> Modify the category set of CHARACTER by adding CATEGORY to it.
    !> The category is changed only for table TABLE, which defaults to
    !> the current buffer's category table.
    !> CHARACTER can be either a single character (FROM) or the
    !> lower and upper ends FROM and TO of an inclusive character range to modify.
    !> CATEGORY must be a category name (a character between ` ' and `~').
    !> If optional argument RESET is true,
    !> then delete CATEGORY from the category set instead of adding it.
    !> Port of category.c:Fmodify_category_entry.
    recursive subroutine modify_category_entry(from, category, status, to, table, reset)
        integer, intent(in) :: from
        character, intent(in) :: category
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: to
        type(handle_type), intent(in), optional :: table
        logical, intent(in), optional :: reset
        type(handle_type) :: h
        type(category_set_type) :: category_set
        type(status_type) :: st
        logical :: set_value
        integer :: info, start, last, f, t, n

        start = from
        last = from
        if (present(to)) last = to

        if (start < 0 .or. start > MAX_CHAR .or. last < 0 .or. last > MAX_CHAR) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer (a character code)")
            return
        end if

        if (.not. categoryp(category)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected category character(len=1)")
            return
        end if

        call check_category_table(h, info, status, table)
        if (.not. status%ok()) return

        if (.not. allocated(infos(info)%docstrings(iachar(category))%text)) then
            call set_error(status, FM_GENERIC_ERROR, "Undefined category: "//category)
            return
        end if

        set_value = .true.
        if (present(reset)) set_value = .not. reset

        do while (start <= last)
            f = start
            t = last
            n = int(char_table_ref_and_range(h, start, f, t))
            category_set = category_set_type()
            if (n >= 1 .and. n <= infos(info)%nsets) category_set = infos(info)%sets(n)

            if (category_member(category, category_set) .neqv. set_value) then
                call set_category_set(category_set, category, set_value)
                n = hash_get_category_set(info, category_set)
                call char_table_set_range(h, start, t, int(n, int64), st)
            end if

            start = t + 1
        end do
    end subroutine modify_category_entry

    !> Make the standard category table: a category set which contains
    !> nothing as its default, then characters.el's categories.  Port of
    !> category.c:init_category_once, with the entries of characters_fm.
    recursive subroutine init_category_once()
        call new_category_char_table(standard)
        call characters_categories(characters_define, characters_entry)
    end subroutine init_category_once

    !> characters_categories's definitions, in the standard table.
    !> Not a port.
    recursive subroutine characters_define(category, docstring)
        character, intent(in) :: category
        character(len=*), intent(in) :: docstring
        type(status_type) :: st

        call define_category(category, docstring, st, standard)
    end subroutine characters_define

    !> characters_categories's entries, in the standard table.  Not a port.
    recursive subroutine characters_entry(from, to, category)
        integer, intent(in) :: from, to
        character, intent(in) :: category
        type(status_type) :: st

        call modify_category_entry(from, category, st, to=to, table=standard)
    end subroutine characters_entry
end module category_fm
