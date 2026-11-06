! fortmacs_widget_for_object_collections_mod.f90 -- utility to maintain a view of a list of objects in a buffer.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/ewoc.el.
!
! Copyright (C) 1991-2026 Free Software Foundation, Inc.
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

!> Ewoc: Emacs's Widget for Object Collections.  An ewoc is a list of
!> data elements shown in a buffer, each printed by a pretty-printer,
!> between a header and a footer.  The elements can be added, deleted,
!> refreshed, located from a buffer position and walked.
!>
!> An ewoc is a handle (`fortmacs_widget_for_object_collections_type'), freed with `destroy_fortmacs_widget_for_object_collections'
!> (there is no
!> GC).  Its nodes are numbers within it, 0 standing for none.  The data
!> of a node is an integer the caller gives meaning to (typically an
!> index into the caller's own array of elements).
module fortmacs_widget_for_object_collections_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use handle_mod, only: handle_type, null_handle, slot_table_type
    use value_mod, only: value_type
    use data_mod, only: symbol_value, set
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, is_buffer_live
    use marker_mod, only: copy_marker_at, marker_position, set_marker, destroy_marker
    use editing_functions_mod, only: point, goto_character, delete_region
    use text_properties_mod, only: put_text_property
    use insert_delete_mod, only: insert
    implicit none(type, external)
    private

    public :: fortmacs_widget_for_object_collections_type, fortmacs_widget_for_object_collections_pp_procedure, &
              fortmacs_widget_for_object_collections_test_procedure, fortmacs_widget_for_object_collections_set_hf_parts
    public :: make_fortmacs_widget_for_object_collections, destroy_fortmacs_widget_for_object_collections, &
              fortmacs_widget_for_object_collections_location, fortmacs_widget_for_object_collections_data, &
              fortmacs_widget_for_object_collections_set_data, fortmacs_widget_for_object_collections_enter_first, &
              fortmacs_widget_for_object_collections_enter_last, &
              fortmacs_widget_for_object_collections_enter_after, fortmacs_widget_for_object_collections_enter_before, &
              fortmacs_widget_for_object_collections_next, fortmacs_widget_for_object_collections_previous, &
              fortmacs_widget_for_object_collections_nth, fortmacs_widget_for_object_collections_map, &
              fortmacs_widget_for_object_collections_delete, &
              fortmacs_widget_for_object_collections_filter, fortmacs_widget_for_object_collections_locate, &
              fortmacs_widget_for_object_collections_invalidate, fortmacs_widget_for_object_collections_goto_previous, &
              fortmacs_widget_for_object_collections_goto_next, fortmacs_widget_for_object_collections_goto_node, &
              fortmacs_widget_for_object_collections_refresh, fortmacs_widget_for_object_collections_collect, &
              fortmacs_widget_for_object_collections_buffer, fortmacs_widget_for_object_collections_get_hf, &
              fortmacs_widget_for_object_collections_set_hf

    !> A part of a header or footer: TEXT with the face FACE ("" for
    !> none), as `propertize' gives it.
    type, public :: fortmacs_widget_for_object_collections_part_type
        character(len=:), allocatable :: text, face
    end type fortmacs_widget_for_object_collections_part_type

    !> An ewoc: a handle into the table of ewocs.
    type :: fortmacs_widget_for_object_collections_type
        type(handle_type) :: h
    end type fortmacs_widget_for_object_collections_type

    abstract interface
        !> A pretty-printer: insert at point a representation of the
        !> element DATA.  Not a port.
        recursive subroutine fortmacs_widget_for_object_collections_pp_procedure(data)
            import :: int64
            implicit none(type, external)
            integer(int64), intent(in) :: data
        end subroutine fortmacs_widget_for_object_collections_pp_procedure

        !> A function of an element DATA, for `fortmacs_widget_for_object_collections_map' (refresh when
        !> true), `fortmacs_widget_for_object_collections_filter' (keep when true) and
        !> `fortmacs_widget_for_object_collections_collect'
        !> (select when true).  Not a port.
        recursive logical function fortmacs_widget_for_object_collections_test_procedure(data)
            import :: int64
            implicit none(type, external)
            integer(int64), intent(in) :: data
        end function fortmacs_widget_for_object_collections_test_procedure
    end interface

    ! A node of the doubly linked list: its neighbors, its data and the
    ! marker at its start.  Node 1 is the list itself (the dummy node,
    ! whose marker is the end of the footer).  The header and the footer
    ! are nodes whose data is their string.
    type :: node_type
        integer :: left = 0, right = 0
        integer(int64) :: data = 0
        type(handle_type) :: start_marker
        logical :: is_hf = .false.
        type(fortmacs_widget_for_object_collections_part_type), allocatable :: hf(:)
    end type node_type

    type :: fortmacs_widget_for_object_collections_data_type
        type(handle_type) :: buffer
        procedure(fortmacs_widget_for_object_collections_pp_procedure), pointer, nopass :: pretty_printer => null()
        logical :: nosep = .false.
        integer :: header = 0, footer = 0, last_node = 0
        type(node_type), allocatable :: nodes(:)
        integer :: n = 0
    end type fortmacs_widget_for_object_collections_data_type

    integer, parameter :: DLL = 1

    type(slot_table_type) :: table
    type(fortmacs_widget_for_object_collections_data_type), allocatable :: fortmacs_widgets_for_object_collections(:)

    character(len=*), parameter :: LF = achar(10)

contains

    ! ------------------------------------------------------------------
    ! The table and the nodes
    ! ------------------------------------------------------------------

    ! The index of FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS in
    ! `fortmacs_widgets_for_object_collections' (0 for a stale handle).
    ! Not a port.
    integer function slot(fortmacs_widget_for_object_collections) result(i)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer(int64) :: k
        type(status_type) :: st

        call table%resolve(fortmacs_widget_for_object_collections%h, k, st)
        i = 0
        if (st%ok()) i = int(k)
    end function slot

    ! A new node of E for DATA (and a header or footer string HF),
    ! starting at POSITION.  Not a port.
    integer function make_node(e, data, position, hf) result(node)
        type(fortmacs_widget_for_object_collections_data_type), intent(inout) :: e
        integer(int64), intent(in) :: data, position
        character(len=*), intent(in), optional :: hf
        type(node_type), allocatable :: grown(:)
        type(status_type) :: st
        integer :: k

        if (.not. allocated(e%nodes)) allocate (e%nodes(8))

        if (e%n == size(e%nodes)) then
            allocate (grown(2*e%n))

            do k = 1, e%n
                grown(k)%left = e%nodes(k)%left
                grown(k)%right = e%nodes(k)%right
                grown(k)%data = e%nodes(k)%data
                grown(k)%start_marker = e%nodes(k)%start_marker
                grown(k)%is_hf = e%nodes(k)%is_hf
                if (allocated(e%nodes(k)%hf)) call move_alloc(e%nodes(k)%hf, grown(k)%hf)
            end do

            call move_alloc(grown, e%nodes)
        end if

        e%n = e%n + 1
        node = e%n
        e%nodes(node)%data = data
        call copy_marker_at(position, e%nodes(node)%start_marker, .false., st)

        if (present(hf)) then
            e%nodes(node)%is_hf = .true.
            allocate (e%nodes(node)%hf(1))
            e%nodes(node)%hf(1)%text = hf
            e%nodes(node)%hf(1)%face = ""
        end if
    end function make_node

    ! The position of NODE's start marker.  Not a port.
    integer(int64) function marked_position(e, node)
        type(fortmacs_widget_for_object_collections_data_type), intent(in) :: e
        integer, intent(in) :: node
        type(status_type) :: st

        call marker_position(e%nodes(node)%start_marker, marked_position, st)
    end function marked_position

    ! Return the node after NODE, or nil if NODE is the last node.  Port of
    ! ewoc.el:ewoc--node-next.
    integer function node_next(e, node) result(r)
        type(fortmacs_widget_for_object_collections_data_type), intent(in) :: e
        integer, intent(in) :: node

        r = e%nodes(node)%right
        if (r == DLL) r = 0
    end function node_next

    ! Return the node before NODE, or nil if NODE is the first node.
    ! Port of ewoc.el:ewoc--node-prev.
    integer function node_previous(e, node) result(l)
        type(fortmacs_widget_for_object_collections_data_type), intent(in) :: e
        integer, intent(in) :: node

        l = e%nodes(node)%left
        if (l == DLL) l = 0
    end function node_previous

    ! Return the Nth node from the doubly linked list.
    ! N counts from zero.  If N is negative, return the -(N+1)th last element.
    ! If N is out of range, return nil.  Port of ewoc.el:ewoc--node-nth.
    integer function node_nth(e, n_in) result(node)
        type(fortmacs_widget_for_object_collections_data_type), intent(in) :: e
        integer, intent(in) :: n_in
        integer :: n
        logical :: left

        left = n_in < 0
        n = n_in

        if (left) then
            n = -1 - n
            node = e%nodes(DLL)%left
        else
            node = e%nodes(DLL)%right
        end if

        do while (node /= DLL .and. n > 0)
            if (left) then
                node = e%nodes(node)%left
            else
                node = e%nodes(node)%right
            end if

            n = n - 1
        end do

        if (node == DLL) node = 0
    end function node_nth

    ! Evaluate NODE once and return it.
    ! BUT if it is the header or the footer in E return nil instead.
    ! Port of ewoc.el:ewoc--filter-hf-nodes.
    integer function filter_hf_nodes(e, node) result(r)
        type(fortmacs_widget_for_object_collections_data_type), intent(in) :: e
        integer, intent(in) :: node

        r = node
        if (node == e%header .or. node == e%footer) r = 0
    end function filter_hf_nodes

    ! "Manually reseat" markers for NODE and its successors (including footer
    ! and dll), in the case where they originally shared start position with
    ! BEGINNING, to END.  BEGINNING and END are buffer positions describing NODE's left
    ! neighbor.  This operation is functionally equivalent to temporarily
    ! setting these nodes' markers' insertion type to t around the pretty-print
    ! call that precedes the call to `ewoc--adjust', and then changing them back
    ! to nil.  Port of ewoc.el:ewoc--adjust.
    subroutine adjust(e, beginning, end, node_in)
        type(fortmacs_widget_for_object_collections_data_type), intent(inout) :: e
        integer(int64), intent(in) :: beginning, end
        integer, intent(in) :: node_in
        integer :: node
        type(status_type) :: st

        if (beginning >= end) return
        node = node_in

        do
            if (marked_position(e, node) /= beginning) exit
            call set_marker(e%nodes(node)%start_marker, end, st)
            ! The "dummy" node `dll' actually holds the marker that
            ! points to the end of the footer, so we check `dll'
            ! *after* reseating the marker.
            if (node == DLL) exit
            node = e%nodes(node)%right
        end do
    end subroutine adjust

    ! Print the data of NODE at point: the header or footer string, or the
    ! pretty-printer's output, then a newline unless NOSEP.  Not a port.
    recursive subroutine print_node(e, node)
        type(fortmacs_widget_for_object_collections_data_type), intent(in) :: e
        integer, intent(in) :: node
        type(status_type) :: st
        integer(int64) :: beginning
        integer :: k

        if (e%nodes(node)%is_hf) then
            do k = 1, size(e%nodes(node)%hf)
                beginning = point()
                call insert(e%nodes(node)%hf(k)%text, st)

                if (len(e%nodes(node)%hf(k)%face) > 0 .and. point() > beginning) then
                    call put_text_property(beginning, point(), value_type(name="face"), &
                                           value_type(name=e%nodes(node)%hf(k)%face), st)
                end if
            end do
            ! The header and footer are always separated by a newline
            ! (hf-pp is (ewoc--wrap #'insert)) unless NOSEP.
        else if (associated(e%pretty_printer)) then
            call e%pretty_printer(e%nodes(node)%data)
        end if

        if (.not. e%nosep) call insert(LF, st)
    end subroutine print_node

    ! Insert before NODE a new node for DATA (or the header or footer HF).
    ! Call the pretty-printer with point at NODE's start, thus pushing back
    ! NODE and leaving the new node's start there.  Return the new node.
    ! Port of ewoc.el:ewoc--insert-new-node.
    recursive integer function insert_new_node(i, node, data, hf) result(elemnode)
        integer, intent(in) :: i, node
        integer(int64), intent(in) :: data
        character(len=*), intent(in), optional :: hf
        type(handle_type) :: pm
        integer(int64) :: position
        integer :: left
        type(status_type) :: st

        call copy_marker_at(point(), pm, .false., st)
        elemnode = make_node(fortmacs_widgets_for_object_collections(i), data, &
                             marked_position(fortmacs_widgets_for_object_collections(i), node), hf)
        left = fortmacs_widgets_for_object_collections(i)%nodes(node)%left
        fortmacs_widgets_for_object_collections(i)%nodes(elemnode)%left = left
        fortmacs_widgets_for_object_collections(i)%nodes(elemnode)%right = node
        fortmacs_widgets_for_object_collections(i)%nodes(left)%right = elemnode
        fortmacs_widgets_for_object_collections(i)%nodes(node)%left = elemnode
        call refresh_node(i, elemnode)
        call marker_position(pm, position, st)
        call goto_character(position)
        call destroy_marker(pm, st)
    end function insert_new_node

    ! Redisplay the element represented by NODE using the pretty-printer.
    ! Port of ewoc.el:ewoc--refresh-node.
    recursive subroutine refresh_node(i, node)
        integer, intent(in) :: i, node
        integer(int64) :: m, end, original_point, offset, position
        integer :: r
        logical :: at_end, has_offset
        type(value_type) :: saved
        type(handle_type) :: pm
        type(status_type) :: st

        m = marked_position(fortmacs_widgets_for_object_collections(i), node)
        r = fortmacs_widgets_for_object_collections(i)%nodes(node)%right
        end = marked_position(fortmacs_widgets_for_object_collections(i), r)
        call symbol_value(value_type(name="inhibit_read_only"), saved, st)
        if (.not. st%ok()) saved = value_type()
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
        original_point = point()
        at_end = original_point == end
        has_offset = .false.
        offset = 0

        if (.not. at_end .and. m < original_point .and. original_point < end) then
            has_offset = .true.
            offset = original_point - m
        end if

        call copy_marker_at(original_point, pm, .false., st)
        ! First, remove the string from the buffer:
        call delete_region(m, end, st)
        ! Calculate and insert the string.
        call goto_character(m)
        call print_node(fortmacs_widgets_for_object_collections(i), node)
        end = point()
        call adjust(fortmacs_widgets_for_object_collections(i), m, point(), r)
        ! (The end of save-excursion.)
        call marker_position(pm, position, st)
        call goto_character(position)
        call destroy_marker(pm, st)

        if (at_end) then
            call goto_character(end)
        else if (has_offset) then
            call goto_character(min(m + offset, end - 1))
        end if

        call set(value_type(name="inhibit_read_only"), saved, st)
    end subroutine refresh_node

    ! with-current-buffer of an ewoc's buffer: make it current, keeping
    ! the old one in OLD.  Not a port.
    subroutine enter_buffer(i, old)
        integer, intent(in) :: i
        type(handle_type), intent(out) :: old
        type(status_type) :: st

        old = current_buffer()
        call set_buffer(fortmacs_widgets_for_object_collections(i)%buffer, st)
    end subroutine enter_buffer

    ! Make OLD current again if it is live.  Not a port.
    subroutine leave_buffer(old)
        type(handle_type), intent(in) :: old
        type(status_type) :: st

        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine leave_buffer

    ! ------------------------------------------------------------------
    ! The public functions
    ! ------------------------------------------------------------------

    !> Create an empty ewoc.
    !>
    !> The ewoc will be inserted in the current buffer at the current position.
    !>
    !> PRETTY_PRINTER should be a procedure that takes one argument, an
    !> element, and inserts a string representing it in the buffer (at
    !> point).  The string PRETTY_PRINTER inserts may be empty or span
    !> several lines.  The PRETTY_PRINTER should use `insert', and not
    !> `insert_before_markers'.
    !>
    !> Optional second and third arguments HEADER and FOOTER are strings,
    !> possibly empty, that will always be present at the top and bottom,
    !> respectively, of the ewoc.
    !>
    !> Normally, a newline is automatically inserted after the header,
    !> the footer and every node's printed representation.  Optional
    !> fourth arg NOSEP true inhibits this.  Port of ewoc.el:ewoc-create.
    recursive function make_fortmacs_widget_for_object_collections(pretty_printer, header, footer, &
                                                                   nosep) result(fortmacs_widget_for_object_collections)
        procedure(fortmacs_widget_for_object_collections_pp_procedure) :: pretty_printer
        character(len=*), intent(in), optional :: header, footer
        logical, intent(in), optional :: nosep
        type(fortmacs_widget_for_object_collections_type) :: fortmacs_widget_for_object_collections
        type(fortmacs_widget_for_object_collections_data_type), allocatable :: grown(:)
        integer(int64) :: k, position
        integer :: i, foot, head
        type(status_type) :: st

        call table%acquire(fortmacs_widget_for_object_collections%h)
        i = int(fortmacs_widget_for_object_collections%h%index)
        if (.not. allocated(fortmacs_widgets_for_object_collections)) allocate (fortmacs_widgets_for_object_collections(0))

        if (size(fortmacs_widgets_for_object_collections, kind=int64) < table%capacity()) then
            allocate (grown(table%capacity()))

            do k = 1, size(fortmacs_widgets_for_object_collections)
                grown(k)%buffer = fortmacs_widgets_for_object_collections(k)%buffer
                grown(k)%pretty_printer => fortmacs_widgets_for_object_collections(k)%pretty_printer
                grown(k)%nosep = fortmacs_widgets_for_object_collections(k)%nosep
                grown(k)%header = fortmacs_widgets_for_object_collections(k)%header
                grown(k)%footer = fortmacs_widgets_for_object_collections(k)%footer
                grown(k)%last_node = fortmacs_widgets_for_object_collections(k)%last_node
                grown(k)%n = fortmacs_widgets_for_object_collections(k)%n
                if (allocated(fortmacs_widgets_for_object_collections(k)%nodes)) call move_alloc( &
                    fortmacs_widgets_for_object_collections(k)%nodes, grown(k)%nodes)
            end do

            call move_alloc(grown, fortmacs_widgets_for_object_collections)
        end if

        fortmacs_widgets_for_object_collections(i) = fortmacs_widget_for_object_collections_data_type()
        fortmacs_widgets_for_object_collections(i)%buffer = current_buffer()
        fortmacs_widgets_for_object_collections(i)%pretty_printer => pretty_printer
        if (present(nosep)) fortmacs_widgets_for_object_collections(i)%nosep = nosep
        position = point()
        ! The dummy node.
        foot = make_node(fortmacs_widgets_for_object_collections(i), 0_int64, position)
        fortmacs_widgets_for_object_collections(i)%nodes(DLL)%left = DLL
        fortmacs_widgets_for_object_collections(i)%nodes(DLL)%right = DLL

        ! Set default values
        if (present(footer)) then
            foot = insert_new_node(i, DLL, 0_int64, footer)
        else
            foot = insert_new_node(i, DLL, 0_int64, "")
        end if

        if (present(header)) then
            head = insert_new_node(i, foot, 0_int64, header)
        else
            head = insert_new_node(i, foot, 0_int64, "")
        end if

        fortmacs_widgets_for_object_collections(i)%footer = foot
        fortmacs_widgets_for_object_collections(i)%header = head
    end function make_fortmacs_widget_for_object_collections

    !> Free FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS and its markers (not a port: upstream relies on GC).  The
    !> text stays in the buffer.  Not a port.
    subroutine destroy_fortmacs_widget_for_object_collections(fortmacs_widget_for_object_collections)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        type(status_type) :: st
        integer :: i, k

        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return

        do k = 1, fortmacs_widgets_for_object_collections(i)%n
            call destroy_marker(fortmacs_widgets_for_object_collections(i)%nodes(k)%start_marker, st)
        end do

        fortmacs_widgets_for_object_collections(i) = fortmacs_widget_for_object_collections_data_type()
        call table%release(fortmacs_widget_for_object_collections%h, st)
    end subroutine destroy_fortmacs_widget_for_object_collections

    !> Return the start location of NODE.  Port of ewoc.el:ewoc-location.
    integer(int64) function fortmacs_widget_for_object_collections_location(fortmacs_widget_for_object_collections, node)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer, intent(in) :: node
        integer :: i

        fortmacs_widget_for_object_collections_location = 0
        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0 .or. node == 0) return
        fortmacs_widget_for_object_collections_location = marked_position(fortmacs_widgets_for_object_collections(i), node)
    end function fortmacs_widget_for_object_collections_location

    !> Extract the data encapsulated by NODE and return it.  Port of
    !> ewoc.el:ewoc-data.
    integer(int64) function fortmacs_widget_for_object_collections_data(fortmacs_widget_for_object_collections, node)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer, intent(in) :: node
        integer :: i

        fortmacs_widget_for_object_collections_data = 0
        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0 .or. node == 0) return
        fortmacs_widget_for_object_collections_data = fortmacs_widgets_for_object_collections(i)%nodes(node)%data
    end function fortmacs_widget_for_object_collections_data

    !> Set NODE to encapsulate DATA.  Port of ewoc.el:ewoc-set-data.
    subroutine fortmacs_widget_for_object_collections_set_data(fortmacs_widget_for_object_collections, node, data)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer, intent(in) :: node
        integer(int64), intent(in) :: data
        integer :: i

        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0 .or. node == 0) return
        fortmacs_widgets_for_object_collections(i)%nodes(node)%data = data
    end subroutine fortmacs_widget_for_object_collections_set_data

    !> Enter DATA first in FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS.
    !> Return the new node.  Port of ewoc.el:ewoc-enter-first.
    recursive integer function fortmacs_widget_for_object_collections_enter_first(fortmacs_widget_for_object_collections, &
                                                                                  data) result(node)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer(int64), intent(in) :: data
        integer :: i

        node = 0
        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        node = fortmacs_widget_for_object_collections_enter_after(fortmacs_widget_for_object_collections, &
                                                                  node_nth(fortmacs_widgets_for_object_collections(i), 0), data)
    end function fortmacs_widget_for_object_collections_enter_first

    !> Enter DATA last in FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS.
    !> Return the new node.  Port of ewoc.el:ewoc-enter-last.
    recursive integer function fortmacs_widget_for_object_collections_enter_last(fortmacs_widget_for_object_collections, &
                                                                                 data) result(node)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer(int64), intent(in) :: data
        integer :: i

        node = 0
        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        node = fortmacs_widget_for_object_collections_enter_before(fortmacs_widget_for_object_collections, &
                                                                   node_nth(fortmacs_widgets_for_object_collections(i), -1), data)
    end function fortmacs_widget_for_object_collections_enter_last

    !> Enter a new element DATA after NODE in FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS.
    !> Return the new node.  Port of ewoc.el:ewoc-enter-after.
    recursive integer function fortmacs_widget_for_object_collections_enter_after(fortmacs_widget_for_object_collections, node, &
                                                                                  data) result(new)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer, intent(in) :: node
        integer(int64), intent(in) :: data
        integer :: i, next

        new = 0
        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        next = node_next(fortmacs_widgets_for_object_collections(i), node)
        if (next == 0) next = DLL
        new = fortmacs_widget_for_object_collections_enter_before(fortmacs_widget_for_object_collections, next, data)
    end function fortmacs_widget_for_object_collections_enter_after

    !> Enter a new element DATA before NODE in FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS.
    !> Return the new node.  Port of ewoc.el:ewoc-enter-before.
    recursive integer function fortmacs_widget_for_object_collections_enter_before(fortmacs_widget_for_object_collections, node, &
                                                                                   data) result(new)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer, intent(in) :: node
        integer(int64), intent(in) :: data
        type(handle_type) :: old
        integer :: i

        new = 0
        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        call enter_buffer(i, old)
        new = insert_new_node(i, node, data)
        call leave_buffer(old)
    end function fortmacs_widget_for_object_collections_enter_before

    !> Return the node in FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS that follows NODE.
    !> Return 0 if NODE is 0 or the last element.  Port of
    !> ewoc.el:ewoc-next.
    integer function fortmacs_widget_for_object_collections_next(fortmacs_widget_for_object_collections, node) result(next)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer, intent(in) :: node
        integer :: i

        next = 0
        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0 .or. node == 0) return
        next = filter_hf_nodes(fortmacs_widgets_for_object_collections(i), node_next(fortmacs_widgets_for_object_collections(i), &
                                                                                     node))
    end function fortmacs_widget_for_object_collections_next

    !> Return the node in FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS that precedes NODE.
    !> Return 0 if NODE is 0 or the first element.  Port of
    !> ewoc.el:ewoc-prev.
    integer function fortmacs_widget_for_object_collections_previous(fortmacs_widget_for_object_collections, node) result(previous)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer, intent(in) :: node
        integer :: i

        previous = 0
        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0 .or. node == 0) return
        previous = filter_hf_nodes(fortmacs_widgets_for_object_collections(i), &
                                   node_previous(fortmacs_widgets_for_object_collections(i), node))
    end function fortmacs_widget_for_object_collections_previous

    !> Return the Nth node.
    !> N counts from zero.  Return 0 if there are less than N elements.
    !> If N is negative, return the -(N+1)th last element.
    !> Thus, fortmacs_widget_for_object_collections_nth(ewoc, 0) returns the first node,
    !> and fortmacs_widget_for_object_collections_nth(ewoc, -1) returns the last node.
    !> Use `fortmacs_widget_for_object_collections_data' to extract the data from the node.  Port of
    !> ewoc.el:ewoc-nth.
    integer function fortmacs_widget_for_object_collections_nth(fortmacs_widget_for_object_collections, n) result(node)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer, intent(in) :: n
        integer :: i

        node = 0
        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return

        ! Skip the header (or footer, if n is negative).
        if (n < 0) then
            node = filter_hf_nodes(fortmacs_widgets_for_object_collections(i), &
                                   node_nth(fortmacs_widgets_for_object_collections(i), n - 1))
        else
            node = filter_hf_nodes(fortmacs_widgets_for_object_collections(i), &
                                   node_nth(fortmacs_widgets_for_object_collections(i), n + 1))
        end if
    end function fortmacs_widget_for_object_collections_nth

    !> Apply MAP_FUNCTION to all elements in FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS.
    !> MAP_FUNCTION is applied to the first element first.
    !> If MAP_FUNCTION returns true the element will be refreshed (its
    !> pretty-printer will be called once again).
    !>
    !> Note that the buffer for FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS will be the current buffer when
    !> MAP_FUNCTION is called.  MAP_FUNCTION must restore the current
    !> buffer before it returns, if it changes it.  Port of
    !> ewoc.el:ewoc-map.
    recursive subroutine fortmacs_widget_for_object_collections_map(map_function, fortmacs_widget_for_object_collections)
        procedure(fortmacs_widget_for_object_collections_test_procedure) :: map_function
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        type(handle_type) :: old
        integer :: i, node

        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        call enter_buffer(i, old)
        node = node_nth(fortmacs_widgets_for_object_collections(i), 1)

        do while (node /= fortmacs_widgets_for_object_collections(i)%footer .and. node /= 0)
            if (map_function(fortmacs_widgets_for_object_collections(i)%nodes(node)%data)) call refresh_node(i, node)
            node = node_next(fortmacs_widgets_for_object_collections(i), node)
        end do

        call leave_buffer(old)
    end subroutine fortmacs_widget_for_object_collections_map

    !> Delete NODES from FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS.  Port of ewoc.el:ewoc-delete.
    subroutine fortmacs_widget_for_object_collections_delete(fortmacs_widget_for_object_collections, nodes)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer, intent(in) :: nodes(:)
        type(handle_type) :: old
        integer :: i, k, node, l, r, next
        type(status_type) :: st

        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        call enter_buffer(i, old)

        do k = 1, size(nodes)
            node = nodes(k)
            if (node == 0) cycle
            ! If we are about to delete the node pointed at by last-node,
            ! set last-node to nil.
            if (fortmacs_widgets_for_object_collections(i)%last_node == node) fortmacs_widgets_for_object_collections( &
                i)%last_node = 0
            next = fortmacs_widgets_for_object_collections(i)%nodes(node)%right
            call delete_region(marked_position(fortmacs_widgets_for_object_collections(i), node), &
                               marked_position(fortmacs_widgets_for_object_collections(i), next), st)
            call destroy_marker(fortmacs_widgets_for_object_collections(i)%nodes(node)%start_marker, st)
            l = fortmacs_widgets_for_object_collections(i)%nodes(node)%left
            r = fortmacs_widgets_for_object_collections(i)%nodes(node)%right
            ! Link neighbors to each other.
            fortmacs_widgets_for_object_collections(i)%nodes(l)%right = r
            fortmacs_widgets_for_object_collections(i)%nodes(r)%left = l
            ! Forget neighbors.
            fortmacs_widgets_for_object_collections(i)%nodes(node)%left = 0
            fortmacs_widgets_for_object_collections(i)%nodes(node)%right = 0
        end do

        call leave_buffer(old)
    end subroutine fortmacs_widget_for_object_collections_delete

    !> Remove all elements in FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS for which TEST returns false.
    !> Note that the buffer for FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS will be the current buffer when TEST
    !> is called.  TEST must restore the current buffer before it returns
    !> if it changes it.
    !> The TEST is called with the element as its first argument.  Port of
    !> ewoc.el:ewoc-filter.
    recursive subroutine fortmacs_widget_for_object_collections_filter(fortmacs_widget_for_object_collections, test)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        procedure(fortmacs_widget_for_object_collections_test_procedure) :: test
        integer, allocatable :: goodbye(:)
        type(handle_type) :: old
        type(value_type) :: saved
        type(status_type) :: st
        integer :: i, node, n

        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        call enter_buffer(i, old)
        call symbol_value(value_type(name="inhibit_read_only"), saved, st)
        if (.not. st%ok()) saved = value_type()
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
        allocate (goodbye(fortmacs_widgets_for_object_collections(i)%n))
        n = 0
        node = node_nth(fortmacs_widgets_for_object_collections(i), 1)

        do while (node /= fortmacs_widgets_for_object_collections(i)%footer .and. node /= 0)
            if (.not. test(fortmacs_widgets_for_object_collections(i)%nodes(node)%data)) then
                ! (push node goodbye)
                n = n + 1
                goodbye(n) = node
            end if

            node = node_next(fortmacs_widgets_for_object_collections(i), node)
        end do

        call leave_buffer(old)
        call fortmacs_widget_for_object_collections_delete(fortmacs_widget_for_object_collections, goodbye(n:1:-1))
        call set(value_type(name="inhibit_read_only"), saved, st)
    end subroutine fortmacs_widget_for_object_collections_filter

    !> Return the node that POSITION (a buffer position) is within.
    !> POSITION defaults to point.
    !> GUESS should be a node that it is likely to be near POSITION.
    !>
    !> If POSITION points before the first element, the first node is returned.
    !> If POSITION points after the last element, the last node is returned.
    !> If the FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS is empty, 0 is returned.  Port of ewoc.el:ewoc-locate.
    integer function fortmacs_widget_for_object_collections_locate(fortmacs_widget_for_object_collections, position_in, &
                                                                   guess) result(node)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer(int64), intent(in), optional :: position_in
        integer, intent(in), optional :: guess
        type(handle_type) :: old
        integer(int64) :: position, distance, d
        integer :: i, best_guess, g

        node = 0
        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        position = point()
        call enter_buffer(i, old)
        if (present(position_in)) position = position_in

        associate (e => fortmacs_widgets_for_object_collections(i))
            if (node_nth(e, 1) == node_nth(e, -1)) then
                ! Nothing present?
                node = 0
            else if (position < marked_position(e, node_nth(e, 2))) then
                ! Before second elem?
                node = node_nth(e, 1)
            else if (position >= marked_position(e, node_nth(e, -2))) then
                ! After one-before-last elem?
                node = node_nth(e, -2)
            else
                ! We now know that pos is within an elem.
                ! Make an educated guess about which of the three known
                ! node'es (the first, the last, or GUESS) is nearest.
                best_guess = node_nth(e, 1)
                distance = abs(position - marked_position(e, best_guess))

                if (present(guess)) then
                    if (guess /= 0) then
                        d = abs(position - marked_position(e, guess))

                        if (d < distance) then
                            distance = d
                            best_guess = guess
                        end if
                    end if
                end if

                g = node_nth(e, -1)        ! Check the last elem
                d = abs(position - marked_position(e, g))

                if (d < distance) then
                    distance = d
                    best_guess = g
                end if

                if (e%last_node /= 0) then  ! Check "previous".
                    g = e%last_node
                    d = abs(position - marked_position(e, g))

                    if (d < distance) then
                        distance = d
                        best_guess = g
                    end if
                end if

                ! best-guess is now a "best guess".
                ! Find the correct node. First determine in which direction
                ! it lies, and then move in that direction until it is found.
                if (position >= marked_position(e, best_guess)) then
                    ! Loop until we are exactly one node too far down...
                    do while (position >= marked_position(e, best_guess))
                        best_guess = node_next(e, best_guess)
                        if (best_guess == 0) best_guess = DLL
                        if (best_guess == DLL) exit
                    end do

                    ! ...and return the previous node.
                    node = node_previous(e, best_guess)
                else
                    ! Pos is before best-guess
                    do while (position < marked_position(e, best_guess))
                        best_guess = node_previous(e, best_guess)
                        if (best_guess == 0) exit
                    end do

                    node = best_guess
                end if
            end if
        end associate

        call leave_buffer(old)
    end function fortmacs_widget_for_object_collections_locate

    !> Call FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS's pretty-printer for each element in NODES.
    !> Delete current text first, thus effecting a "refresh".  Port of
    !> ewoc.el:ewoc-invalidate.
    recursive subroutine fortmacs_widget_for_object_collections_invalidate(fortmacs_widget_for_object_collections, nodes)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer, intent(in) :: nodes(:)
        type(handle_type) :: old
        integer :: i, k

        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        call enter_buffer(i, old)

        do k = 1, size(nodes)
            if (nodes(k) /= 0) call refresh_node(i, nodes(k))
        end do

        call leave_buffer(old)
    end subroutine fortmacs_widget_for_object_collections_invalidate

    !> Move point to the ARGth previous element in FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS.
    !> Don't move if we are at the first element, or if FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS is empty.
    !> Return the node we moved to.  Port of ewoc.el:ewoc-goto-prev.
    integer function fortmacs_widget_for_object_collections_goto_previous(fortmacs_widget_for_object_collections, &
                                                                          argument_in) result(node)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer, intent(in) :: argument_in
        type(handle_type) :: old
        integer :: i, argument

        node = 0
        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        argument = argument_in
        node = fortmacs_widget_for_object_collections_locate(fortmacs_widget_for_object_collections, point())
        call enter_buffer(i, old)

        if (node /= 0) then
            ! If we were past the last element, first jump to it.
            if (point() >= marked_position(fortmacs_widgets_for_object_collections(i), &
                                           fortmacs_widgets_for_object_collections(i)%nodes(node)%right)) argument = argument - 1

            do while (node /= 0 .and. argument > 0)
                argument = argument - 1
                node = node_previous(fortmacs_widgets_for_object_collections(i), node)
            end do

            ! Never step above the first element.
            if (filter_hf_nodes(fortmacs_widgets_for_object_collections(i), &
                                node) == 0) node = node_nth(fortmacs_widgets_for_object_collections(i), 1)
            call leave_buffer(old)
            call fortmacs_widget_for_object_collections_goto_node(fortmacs_widget_for_object_collections, node)
        else
            call leave_buffer(old)
        end if
    end function fortmacs_widget_for_object_collections_goto_previous

    !> Move point to the ARGth next element in FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS.
    !> Return the node (or 0 if we just passed the last node).  Port of
    !> ewoc.el:ewoc-goto-next.
    integer function fortmacs_widget_for_object_collections_goto_next(fortmacs_widget_for_object_collections, argument_in, &
                                                                      status) result(node)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer, intent(in) :: argument_in
        type(status_type), intent(out) :: status
        integer :: i, argument

        status = status_type()
        node = 0
        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        argument = argument_in
        node = fortmacs_widget_for_object_collections_locate(fortmacs_widget_for_object_collections, point())

        do while (node /= 0 .and. argument > 0)
            argument = argument - 1
            node = node_next(fortmacs_widgets_for_object_collections(i), node)
        end do

        ! Never step below the first element.
        ! (unless (ewoc--filter-hf-nodes ewoc node)
        !   (setq node (ewoc--node-nth dll -2)))
        if (node == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "No next")
            return
        end if

        call fortmacs_widget_for_object_collections_goto_node(fortmacs_widget_for_object_collections, node)
    end function fortmacs_widget_for_object_collections_goto_next

    !> Move point to NODE in FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS.  Port of ewoc.el:ewoc-goto-node.
    subroutine fortmacs_widget_for_object_collections_goto_node(fortmacs_widget_for_object_collections, node)
        use simple_mod, only: goal_column
        use indent_mod, only: move_to_column
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        integer, intent(in) :: node
        type(handle_type) :: old
        integer(int64) :: reached
        type(status_type) :: st
        integer :: i

        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0 .or. node == 0) return
        call enter_buffer(i, old)
        call goto_character(marked_position(fortmacs_widgets_for_object_collections(i), node))
        if (goal_column >= 0) call move_to_column(goal_column, reached, .false., st)
        fortmacs_widgets_for_object_collections(i)%last_node = node
        call leave_buffer(old)
    end subroutine fortmacs_widget_for_object_collections_goto_node

    !> Refresh all data in FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS.
    !> The pretty-printer that was specified when the FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS was created
    !> will be called for all elements in FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS.
    !> Note that `fortmacs_widget_for_object_collections_invalidate' is more efficient if only a small
    !> number of elements needs to be refreshed.  Port of
    !> ewoc.el:ewoc-refresh.
    recursive subroutine fortmacs_widget_for_object_collections_refresh(fortmacs_widget_for_object_collections)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        type(handle_type) :: old
        type(value_type) :: saved
        type(status_type) :: st
        integer :: i, footer, node

        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        call enter_buffer(i, old)
        footer = fortmacs_widgets_for_object_collections(i)%footer
        call symbol_value(value_type(name="inhibit_read_only"), saved, st)
        if (.not. st%ok()) saved = value_type()
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
        call delete_region(marked_position(fortmacs_widgets_for_object_collections(i), &
                                           node_nth(fortmacs_widgets_for_object_collections(i), 1)), &
                           marked_position(fortmacs_widgets_for_object_collections(i), footer), st)
        call goto_character(marked_position(fortmacs_widgets_for_object_collections(i), footer))
        node = node_nth(fortmacs_widgets_for_object_collections(i), 1)

        do while (node /= footer .and. node /= 0)
            call set_marker(fortmacs_widgets_for_object_collections(i)%nodes(node)%start_marker, point(), st)
            call print_node(fortmacs_widgets_for_object_collections(i), node)
            node = node_next(fortmacs_widgets_for_object_collections(i), node)
        end do

        call set(value_type(name="inhibit_read_only"), saved, st)
        call set_marker(fortmacs_widgets_for_object_collections(i)%nodes(footer)%start_marker, point(), st)
        call leave_buffer(old)
    end subroutine fortmacs_widget_for_object_collections_refresh

    !> Select elements from FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS using TEST.
    !> Return a list of all selected data elements.
    !> TEST is a function that takes a data element as its first
    !> argument.  The elements on the returned list will appear in the
    !> same order as in the buffer.  You should not rely on the order of
    !> calls to TEST.
    !> Note that the buffer the FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS is displayed in is the current
    !> buffer when TEST is called.  TEST must restore it if it
    !> changes it.  Port of ewoc.el:ewoc-collect.
    recursive subroutine fortmacs_widget_for_object_collections_collect(fortmacs_widget_for_object_collections, test, result)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        procedure(fortmacs_widget_for_object_collections_test_procedure) :: test
        integer(int64), allocatable, intent(out) :: result(:)
        integer(int64), allocatable :: found(:)
        type(handle_type) :: old
        integer :: i, node, n

        allocate (result(0))
        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        call enter_buffer(i, old)
        allocate (found(fortmacs_widgets_for_object_collections(i)%n))
        n = 0
        node = node_nth(fortmacs_widgets_for_object_collections(i), -2)

        do while (node /= fortmacs_widgets_for_object_collections(i)%header .and. node /= 0)
            if (test(fortmacs_widgets_for_object_collections(i)%nodes(node)%data)) then
                n = n + 1
                found(n) = fortmacs_widgets_for_object_collections(i)%nodes(node)%data
            end if

            node = node_previous(fortmacs_widgets_for_object_collections(i), node)
        end do

        ! (push ...): the first found ends last.
        result = found(n:1:-1)
        call leave_buffer(old)
    end subroutine fortmacs_widget_for_object_collections_collect

    !> Return the buffer that is associated with FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS.
    !> Return the null handle if the buffer has been deleted.  Port of
    !> ewoc.el:ewoc-buffer.
    function fortmacs_widget_for_object_collections_buffer(fortmacs_widget_for_object_collections) result(buffer)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        type(handle_type) :: buffer
        integer :: i

        buffer = null_handle()
        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        if (is_buffer_live(fortmacs_widgets_for_object_collections(i)%buffer)) buffer = fortmacs_widgets_for_object_collections( &
                                                                                        i)%buffer
    end function fortmacs_widget_for_object_collections_buffer

    !> Return the HEADER and FOOTER of FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS.  Port of ewoc.el:ewoc-get-hf.
    subroutine fortmacs_widget_for_object_collections_get_hf(fortmacs_widget_for_object_collections, header, footer)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        character(len=:), allocatable, intent(out) :: header, footer
        integer :: i

        header = ""
        footer = ""
        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        header = text_of(fortmacs_widgets_for_object_collections(i)%nodes(fortmacs_widgets_for_object_collections(i)%header)%hf)
        footer = text_of(fortmacs_widgets_for_object_collections(i)%nodes(fortmacs_widgets_for_object_collections(i)%footer)%hf)

    contains

        function text_of(parts) result(text)
            type(fortmacs_widget_for_object_collections_part_type), intent(in) :: parts(:)
            character(len=:), allocatable :: text
            integer :: k

            text = ""

            do k = 1, size(parts)
                text = text//parts(k)%text
            end do
        end function text_of
    end subroutine fortmacs_widget_for_object_collections_get_hf

    !> Set the HEADER and FOOTER of FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS.  Port of ewoc.el:ewoc-set-hf.
    recursive subroutine fortmacs_widget_for_object_collections_set_hf(fortmacs_widget_for_object_collections, header, footer)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        character(len=*), intent(in) :: header, footer
        type(handle_type) :: old
        integer :: i

        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        call enter_buffer(i, old)
        call set_text(fortmacs_widgets_for_object_collections(i)%nodes(fortmacs_widgets_for_object_collections(i)%header)%hf, &
                      header)
        call set_text(fortmacs_widgets_for_object_collections(i)%nodes(fortmacs_widgets_for_object_collections(i)%footer)%hf, &
                      footer)
        call refresh_node(i, fortmacs_widgets_for_object_collections(i)%header)
        call refresh_node(i, fortmacs_widgets_for_object_collections(i)%footer)
        call leave_buffer(old)

    contains

        subroutine set_text(parts, text)
            type(fortmacs_widget_for_object_collections_part_type), allocatable, intent(inout) :: parts(:)
            character(len=*), intent(in) :: text

            if (allocated(parts)) deallocate (parts)
            allocate (parts(1))
            parts(1)%text = text
            parts(1)%face = ""
        end subroutine set_text
    end subroutine fortmacs_widget_for_object_collections_set_hf

    !> Set the HEADER and FOOTER of FORTMACS_WIDGET_FOR_OBJECT_COLLECTIONS, made of parts with faces (as
    !> `propertize'd strings are).  Port of ewoc.el:ewoc-set-hf.
    recursive subroutine fortmacs_widget_for_object_collections_set_hf_parts(fortmacs_widget_for_object_collections, header, footer)
        type(fortmacs_widget_for_object_collections_type), intent(in) :: fortmacs_widget_for_object_collections
        type(fortmacs_widget_for_object_collections_part_type), intent(in) :: header(:), footer(:)
        type(handle_type) :: old
        integer :: i

        i = slot(fortmacs_widget_for_object_collections)
        if (i == 0) return
        call enter_buffer(i, old)
        fortmacs_widgets_for_object_collections(i)%nodes(fortmacs_widgets_for_object_collections(i)%header)%hf = header
        fortmacs_widgets_for_object_collections(i)%nodes(fortmacs_widgets_for_object_collections(i)%footer)%hf = footer
        call refresh_node(i, fortmacs_widgets_for_object_collections(i)%header)
        call refresh_node(i, fortmacs_widgets_for_object_collections(i)%footer)
        call leave_buffer(old)
    end subroutine fortmacs_widget_for_object_collections_set_hf_parts
end module fortmacs_widget_for_object_collections_mod
