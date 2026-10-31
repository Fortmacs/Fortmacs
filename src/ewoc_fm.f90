! ewoc_fm.f90 -- utility to maintain a view of a list of objects in a buffer.
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
!> An ewoc is a handle (`ewoc_type'), freed with `ewoc_free' (there is no
!> GC).  Its nodes are numbers within it, 0 standing for nil.  The data
!> of a node is an integer the caller gives meaning to (typically an
!> index into the caller's own array of elements).
module ewoc_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type, null_handle, slot_table_type
    use lisp_fm, only: value_type, t_value, nil
    use lread_fm, only: sym
    use data_fm, only: symbol_value, set
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, is_buffer_live
    use marker_fm, only: copy_marker_at, marker_position, set_marker, free_marker
    use editfns_fm, only: point, goto_char, delete_region
    use textprop_fm, only: put_text_property
    use insdel_fm, only: insert
    implicit none(type, external)
    private

    public :: ewoc_type, ewoc_pp_proc, ewoc_predicate_proc, ewoc_set_hf_parts
    public :: ewoc_create, ewoc_free, ewoc_location, ewoc_data, ewoc_set_data, ewoc_enter_first, ewoc_enter_last, &
              ewoc_enter_after, ewoc_enter_before, ewoc_next, ewoc_prev, ewoc_nth, ewoc_map, ewoc_delete, &
              ewoc_filter, ewoc_locate, ewoc_invalidate, ewoc_goto_prev, ewoc_goto_next, ewoc_goto_node, &
              ewoc_refresh, ewoc_collect, ewoc_buffer, ewoc_get_hf, ewoc_set_hf

    !> A part of a header or footer: TEXT with the face FACE ("" for
    !> none), as `propertize' gives it.
    type, public :: ewoc_part_type
        character(len=:), allocatable :: text, face
    end type ewoc_part_type

    !> An ewoc: a handle into the table of ewocs.
    type :: ewoc_type
        type(handle_type) :: h
    end type ewoc_type

    abstract interface
        !> A pretty-printer: insert at point a representation of the
        !> element DATA.  Not a port.
        recursive subroutine ewoc_pp_proc(data)
            import :: int64
            implicit none(type, external)
            integer(int64), intent(in) :: data
        end subroutine ewoc_pp_proc

        !> A function of an element DATA, for `ewoc_map' (refresh when
        !> true), `ewoc_filter' (keep when true) and `ewoc_collect'
        !> (select when true).  Not a port.
        recursive logical function ewoc_predicate_proc(data)
            import :: int64
            implicit none(type, external)
            integer(int64), intent(in) :: data
        end function ewoc_predicate_proc
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
        type(ewoc_part_type), allocatable :: hf(:)
    end type node_type

    type :: ewoc_data_type
        type(handle_type) :: buffer
        procedure(ewoc_pp_proc), pointer, nopass :: pretty_printer => null()
        logical :: nosep = .false.
        integer :: header = 0, footer = 0, last_node = 0
        type(node_type), allocatable :: nodes(:)
        integer :: n = 0
    end type ewoc_data_type

    integer, parameter :: DLL = 1

    type(slot_table_type) :: table
    type(ewoc_data_type), allocatable :: ewocs(:)

    character(len=*), parameter :: LF = achar(10)

contains

    ! ------------------------------------------------------------------
    ! The table and the nodes
    ! ------------------------------------------------------------------

    ! The index of EWOC in `ewocs' (0 for a stale handle).  Not a port.
    integer function slot(ewoc) result(i)
        type(ewoc_type), intent(in) :: ewoc
        integer(int64) :: k
        type(status_type) :: st

        call table%resolve(ewoc%h, k, st)
        i = 0
        if (st%ok()) i = int(k)
    end function slot

    ! A new node of E for DATA (and a header or footer string HF),
    ! starting at POS.  Not a port.
    integer function new_node(e, data, pos, hf) result(node)
        type(ewoc_data_type), intent(inout) :: e
        integer(int64), intent(in) :: data, pos
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
        call copy_marker_at(pos, e%nodes(node)%start_marker, .false., st)

        if (present(hf)) then
            e%nodes(node)%is_hf = .true.
            allocate (e%nodes(node)%hf(1))
            e%nodes(node)%hf(1)%text = hf
            e%nodes(node)%hf(1)%face = ""
        end if
    end function new_node

    ! The position of NODE's start marker.  Not a port.
    integer(int64) function mpos(e, node)
        type(ewoc_data_type), intent(in) :: e
        integer, intent(in) :: node
        type(status_type) :: st

        call marker_position(e%nodes(node)%start_marker, mpos, st)
    end function mpos

    ! Return the node after NODE, or nil if NODE is the last node.  Port of
    ! ewoc.el:ewoc--node-next.
    integer function node_next(e, node) result(r)
        type(ewoc_data_type), intent(in) :: e
        integer, intent(in) :: node

        r = e%nodes(node)%right
        if (r == DLL) r = 0
    end function node_next

    ! Return the node before NODE, or nil if NODE is the first node.
    ! Port of ewoc.el:ewoc--node-prev.
    integer function node_prev(e, node) result(l)
        type(ewoc_data_type), intent(in) :: e
        integer, intent(in) :: node

        l = e%nodes(node)%left
        if (l == DLL) l = 0
    end function node_prev

    ! Return the Nth node from the doubly linked list.
    ! N counts from zero.  If N is negative, return the -(N+1)th last element.
    ! If N is out of range, return nil.  Port of ewoc.el:ewoc--node-nth.
    integer function node_nth(e, n_in) result(node)
        type(ewoc_data_type), intent(in) :: e
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
        type(ewoc_data_type), intent(in) :: e
        integer, intent(in) :: node

        r = node
        if (node == e%header .or. node == e%footer) r = 0
    end function filter_hf_nodes

    ! "Manually reseat" markers for NODE and its successors (including footer
    ! and dll), in the case where they originally shared start position with
    ! BEG, to END.  BEG and END are buffer positions describing NODE's left
    ! neighbor.  This operation is functionally equivalent to temporarily
    ! setting these nodes' markers' insertion type to t around the pretty-print
    ! call that precedes the call to `ewoc--adjust', and then changing them back
    ! to nil.  Port of ewoc.el:ewoc--adjust.
    subroutine adjust(e, beg, end, node_in)
        type(ewoc_data_type), intent(inout) :: e
        integer(int64), intent(in) :: beg, end
        integer, intent(in) :: node_in
        integer :: node
        type(status_type) :: st

        if (beg >= end) return
        node = node_in

        do
            if (mpos(e, node) /= beg) exit
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
        type(ewoc_data_type), intent(in) :: e
        integer, intent(in) :: node
        type(status_type) :: st
        integer(int64) :: beg
        integer :: k

        if (e%nodes(node)%is_hf) then
            do k = 1, size(e%nodes(node)%hf)
                beg = point()
                call insert(e%nodes(node)%hf(k)%text, st)

                if (len(e%nodes(node)%hf(k)%face) > 0 .and. point() > beg) then
                    call put_text_property(beg, point(), sym("face"), sym(e%nodes(node)%hf(k)%face), st)
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
        integer(int64) :: pos
        integer :: left
        type(status_type) :: st

        call copy_marker_at(point(), pm, .false., st)
        elemnode = new_node(ewocs(i), data, mpos(ewocs(i), node), hf)
        left = ewocs(i)%nodes(node)%left
        ewocs(i)%nodes(elemnode)%left = left
        ewocs(i)%nodes(elemnode)%right = node
        ewocs(i)%nodes(left)%right = elemnode
        ewocs(i)%nodes(node)%left = elemnode
        call refresh_node(i, elemnode)
        call marker_position(pm, pos, st)
        call goto_char(pos)
        call free_marker(pm, st)
    end function insert_new_node

    ! Redisplay the element represented by NODE using the pretty-printer.
    ! Port of ewoc.el:ewoc--refresh-node.
    recursive subroutine refresh_node(i, node)
        integer, intent(in) :: i, node
        integer(int64) :: m, end, opoint, offset, pos
        integer :: r
        logical :: at_end, has_offset
        type(value_type) :: saved
        type(handle_type) :: pm
        type(status_type) :: st

        m = mpos(ewocs(i), node)
        r = ewocs(i)%nodes(node)%right
        end = mpos(ewocs(i), r)
        call symbol_value(sym("inhibit_read_only"), saved, st)
        if (.not. st%ok()) saved = nil()
        call set(sym("inhibit_read_only"), t_value(), st)
        opoint = point()
        at_end = opoint == end
        has_offset = .false.
        offset = 0

        if (.not. at_end .and. m < opoint .and. opoint < end) then
            has_offset = .true.
            offset = opoint - m
        end if

        call copy_marker_at(opoint, pm, .false., st)
        ! First, remove the string from the buffer:
        call delete_region(m, end, st)
        ! Calculate and insert the string.
        call goto_char(m)
        call print_node(ewocs(i), node)
        end = point()
        call adjust(ewocs(i), m, point(), r)
        ! (The end of save-excursion.)
        call marker_position(pm, pos, st)
        call goto_char(pos)
        call free_marker(pm, st)

        if (at_end) then
            call goto_char(end)
        else if (has_offset) then
            call goto_char(min(m + offset, end - 1))
        end if

        call set(sym("inhibit_read_only"), saved, st)
    end subroutine refresh_node

    ! with-current-buffer of an ewoc's buffer: make it current, keeping
    ! the old one in OLD.  Not a port.
    subroutine enter_buffer(i, old)
        integer, intent(in) :: i
        type(handle_type), intent(out) :: old
        type(status_type) :: st

        old = current_buffer()
        call set_buffer(ewocs(i)%buffer, st)
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
    recursive function ewoc_create(pretty_printer, header, footer, nosep) result(ewoc)
        procedure(ewoc_pp_proc) :: pretty_printer
        character(len=*), intent(in), optional :: header, footer
        logical, intent(in), optional :: nosep
        type(ewoc_type) :: ewoc
        type(ewoc_data_type), allocatable :: grown(:)
        integer(int64) :: k, pos
        integer :: i, foot, head
        type(status_type) :: st

        call table%acquire(ewoc%h)
        i = int(ewoc%h%index)
        if (.not. allocated(ewocs)) allocate (ewocs(0))

        if (size(ewocs, kind=int64) < table%capacity()) then
            allocate (grown(table%capacity()))

            do k = 1, size(ewocs)
                grown(k)%buffer = ewocs(k)%buffer
                grown(k)%pretty_printer => ewocs(k)%pretty_printer
                grown(k)%nosep = ewocs(k)%nosep
                grown(k)%header = ewocs(k)%header
                grown(k)%footer = ewocs(k)%footer
                grown(k)%last_node = ewocs(k)%last_node
                grown(k)%n = ewocs(k)%n
                if (allocated(ewocs(k)%nodes)) call move_alloc(ewocs(k)%nodes, grown(k)%nodes)
            end do

            call move_alloc(grown, ewocs)
        end if

        ewocs(i) = ewoc_data_type()
        ewocs(i)%buffer = current_buffer()
        ewocs(i)%pretty_printer => pretty_printer
        if (present(nosep)) ewocs(i)%nosep = nosep
        pos = point()
        ! The dummy node.
        foot = new_node(ewocs(i), 0_int64, pos)
        ewocs(i)%nodes(DLL)%left = DLL
        ewocs(i)%nodes(DLL)%right = DLL

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

        ewocs(i)%footer = foot
        ewocs(i)%header = head
    end function ewoc_create

    !> Free EWOC and its markers (not a port: upstream relies on GC).  The
    !> text stays in the buffer.  Not a port.
    subroutine ewoc_free(ewoc)
        type(ewoc_type), intent(in) :: ewoc
        type(status_type) :: st
        integer :: i, k

        i = slot(ewoc)
        if (i == 0) return

        do k = 1, ewocs(i)%n
            call free_marker(ewocs(i)%nodes(k)%start_marker, st)
        end do

        ewocs(i) = ewoc_data_type()
        call table%release(ewoc%h, st)
    end subroutine ewoc_free

    !> Return the start location of NODE.  Port of ewoc.el:ewoc-location.
    integer(int64) function ewoc_location(ewoc, node)
        type(ewoc_type), intent(in) :: ewoc
        integer, intent(in) :: node
        integer :: i

        ewoc_location = 0
        i = slot(ewoc)
        if (i == 0 .or. node == 0) return
        ewoc_location = mpos(ewocs(i), node)
    end function ewoc_location

    !> Extract the data encapsulated by NODE and return it.  Port of
    !> ewoc.el:ewoc-data.
    integer(int64) function ewoc_data(ewoc, node)
        type(ewoc_type), intent(in) :: ewoc
        integer, intent(in) :: node
        integer :: i

        ewoc_data = 0
        i = slot(ewoc)
        if (i == 0 .or. node == 0) return
        ewoc_data = ewocs(i)%nodes(node)%data
    end function ewoc_data

    !> Set NODE to encapsulate DATA.  Port of ewoc.el:ewoc-set-data.
    subroutine ewoc_set_data(ewoc, node, data)
        type(ewoc_type), intent(in) :: ewoc
        integer, intent(in) :: node
        integer(int64), intent(in) :: data
        integer :: i

        i = slot(ewoc)
        if (i == 0 .or. node == 0) return
        ewocs(i)%nodes(node)%data = data
    end subroutine ewoc_set_data

    !> Enter DATA first in EWOC.
    !> Return the new node.  Port of ewoc.el:ewoc-enter-first.
    recursive integer function ewoc_enter_first(ewoc, data) result(node)
        type(ewoc_type), intent(in) :: ewoc
        integer(int64), intent(in) :: data
        integer :: i

        node = 0
        i = slot(ewoc)
        if (i == 0) return
        node = ewoc_enter_after(ewoc, node_nth(ewocs(i), 0), data)
    end function ewoc_enter_first

    !> Enter DATA last in EWOC.
    !> Return the new node.  Port of ewoc.el:ewoc-enter-last.
    recursive integer function ewoc_enter_last(ewoc, data) result(node)
        type(ewoc_type), intent(in) :: ewoc
        integer(int64), intent(in) :: data
        integer :: i

        node = 0
        i = slot(ewoc)
        if (i == 0) return
        node = ewoc_enter_before(ewoc, node_nth(ewocs(i), -1), data)
    end function ewoc_enter_last

    !> Enter a new element DATA after NODE in EWOC.
    !> Return the new node.  Port of ewoc.el:ewoc-enter-after.
    recursive integer function ewoc_enter_after(ewoc, node, data) result(new)
        type(ewoc_type), intent(in) :: ewoc
        integer, intent(in) :: node
        integer(int64), intent(in) :: data
        integer :: i, next

        new = 0
        i = slot(ewoc)
        if (i == 0) return
        next = node_next(ewocs(i), node)
        if (next == 0) next = DLL
        new = ewoc_enter_before(ewoc, next, data)
    end function ewoc_enter_after

    !> Enter a new element DATA before NODE in EWOC.
    !> Return the new node.  Port of ewoc.el:ewoc-enter-before.
    recursive integer function ewoc_enter_before(ewoc, node, data) result(new)
        type(ewoc_type), intent(in) :: ewoc
        integer, intent(in) :: node
        integer(int64), intent(in) :: data
        type(handle_type) :: old
        integer :: i

        new = 0
        i = slot(ewoc)
        if (i == 0) return
        call enter_buffer(i, old)
        new = insert_new_node(i, node, data)
        call leave_buffer(old)
    end function ewoc_enter_before

    !> Return the node in EWOC that follows NODE.
    !> Return nil if NODE is nil or the last element.  Port of
    !> ewoc.el:ewoc-next.
    integer function ewoc_next(ewoc, node) result(next)
        type(ewoc_type), intent(in) :: ewoc
        integer, intent(in) :: node
        integer :: i

        next = 0
        i = slot(ewoc)
        if (i == 0 .or. node == 0) return
        next = filter_hf_nodes(ewocs(i), node_next(ewocs(i), node))
    end function ewoc_next

    !> Return the node in EWOC that precedes NODE.
    !> Return nil if NODE is nil or the first element.  Port of
    !> ewoc.el:ewoc-prev.
    integer function ewoc_prev(ewoc, node) result(prev)
        type(ewoc_type), intent(in) :: ewoc
        integer, intent(in) :: node
        integer :: i

        prev = 0
        i = slot(ewoc)
        if (i == 0 .or. node == 0) return
        prev = filter_hf_nodes(ewocs(i), node_prev(ewocs(i), node))
    end function ewoc_prev

    !> Return the Nth node.
    !> N counts from zero.  Return nil if there is less than N elements.
    !> If N is negative, return the -(N+1)th last element.
    !> Thus, ewoc_nth(ewoc, 0) returns the first node,
    !> and ewoc_nth(ewoc, -1) returns the last node.
    !> Use `ewoc_data' to extract the data from the node.  Port of
    !> ewoc.el:ewoc-nth.
    integer function ewoc_nth(ewoc, n) result(node)
        type(ewoc_type), intent(in) :: ewoc
        integer, intent(in) :: n
        integer :: i

        node = 0
        i = slot(ewoc)
        if (i == 0) return

        ! Skip the header (or footer, if n is negative).
        if (n < 0) then
            node = filter_hf_nodes(ewocs(i), node_nth(ewocs(i), n - 1))
        else
            node = filter_hf_nodes(ewocs(i), node_nth(ewocs(i), n + 1))
        end if
    end function ewoc_nth

    !> Apply MAP_FUNCTION to all elements in EWOC.
    !> MAP_FUNCTION is applied to the first element first.
    !> If MAP_FUNCTION returns true the element will be refreshed (its
    !> pretty-printer will be called once again).
    !>
    !> Note that the buffer for EWOC will be the current buffer when
    !> MAP_FUNCTION is called.  MAP_FUNCTION must restore the current
    !> buffer before it returns, if it changes it.  Port of
    !> ewoc.el:ewoc-map.
    recursive subroutine ewoc_map(map_function, ewoc)
        procedure(ewoc_predicate_proc) :: map_function
        type(ewoc_type), intent(in) :: ewoc
        type(handle_type) :: old
        integer :: i, node

        i = slot(ewoc)
        if (i == 0) return
        call enter_buffer(i, old)
        node = node_nth(ewocs(i), 1)

        do while (node /= ewocs(i)%footer .and. node /= 0)
            if (map_function(ewocs(i)%nodes(node)%data)) call refresh_node(i, node)
            node = node_next(ewocs(i), node)
        end do

        call leave_buffer(old)
    end subroutine ewoc_map

    !> Delete NODES from EWOC.  Port of ewoc.el:ewoc-delete.
    subroutine ewoc_delete(ewoc, nodes)
        type(ewoc_type), intent(in) :: ewoc
        integer, intent(in) :: nodes(:)
        type(handle_type) :: old
        integer :: i, k, node, l, r, next
        type(status_type) :: st

        i = slot(ewoc)
        if (i == 0) return
        call enter_buffer(i, old)

        do k = 1, size(nodes)
            node = nodes(k)
            if (node == 0) cycle
            ! If we are about to delete the node pointed at by last-node,
            ! set last-node to nil.
            if (ewocs(i)%last_node == node) ewocs(i)%last_node = 0
            next = ewocs(i)%nodes(node)%right
            call delete_region(mpos(ewocs(i), node), mpos(ewocs(i), next), st)
            call free_marker(ewocs(i)%nodes(node)%start_marker, st)
            l = ewocs(i)%nodes(node)%left
            r = ewocs(i)%nodes(node)%right
            ! Link neighbors to each other.
            ewocs(i)%nodes(l)%right = r
            ewocs(i)%nodes(r)%left = l
            ! Forget neighbors.
            ewocs(i)%nodes(node)%left = 0
            ewocs(i)%nodes(node)%right = 0
        end do

        call leave_buffer(old)
    end subroutine ewoc_delete

    !> Remove all elements in EWOC for which PREDICATE returns false.
    !> Note that the buffer for EWOC will be the current buffer when PREDICATE
    !> is called.  PREDICATE must restore the current buffer before it returns
    !> if it changes it.
    !> The PREDICATE is called with the element as its first argument.  Port of
    !> ewoc.el:ewoc-filter.
    recursive subroutine ewoc_filter(ewoc, predicate)
        type(ewoc_type), intent(in) :: ewoc
        procedure(ewoc_predicate_proc) :: predicate
        integer, allocatable :: goodbye(:)
        type(handle_type) :: old
        type(value_type) :: saved
        type(status_type) :: st
        integer :: i, node, n

        i = slot(ewoc)
        if (i == 0) return
        call enter_buffer(i, old)
        call symbol_value(sym("inhibit_read_only"), saved, st)
        if (.not. st%ok()) saved = nil()
        call set(sym("inhibit_read_only"), t_value(), st)
        allocate (goodbye(ewocs(i)%n))
        n = 0
        node = node_nth(ewocs(i), 1)

        do while (node /= ewocs(i)%footer .and. node /= 0)
            if (.not. predicate(ewocs(i)%nodes(node)%data)) then
                ! (push node goodbye)
                n = n + 1
                goodbye(n) = node
            end if

            node = node_next(ewocs(i), node)
        end do

        call leave_buffer(old)
        call ewoc_delete(ewoc, goodbye(n:1:-1))
        call set(sym("inhibit_read_only"), saved, st)
    end subroutine ewoc_filter

    !> Return the node that POS (a buffer position) is within.
    !> POS defaults to point.
    !> GUESS should be a node that it is likely to be near POS.
    !>
    !> If POS points before the first element, the first node is returned.
    !> If POS points after the last element, the last node is returned.
    !> If the EWOC is empty, nil is returned.  Port of ewoc.el:ewoc-locate.
    integer function ewoc_locate(ewoc, pos_in, guess) result(node)
        type(ewoc_type), intent(in) :: ewoc
        integer(int64), intent(in), optional :: pos_in
        integer, intent(in), optional :: guess
        type(handle_type) :: old
        integer(int64) :: pos, distance, d
        integer :: i, best_guess, g

        node = 0
        i = slot(ewoc)
        if (i == 0) return
        pos = point()
        call enter_buffer(i, old)
        if (present(pos_in)) pos = pos_in

        associate (e => ewocs(i))
            if (node_nth(e, 1) == node_nth(e, -1)) then
                ! Nothing present?
                node = 0
            else if (pos < mpos(e, node_nth(e, 2))) then
                ! Before second elem?
                node = node_nth(e, 1)
            else if (pos >= mpos(e, node_nth(e, -2))) then
                ! After one-before-last elem?
                node = node_nth(e, -2)
            else
                ! We now know that pos is within an elem.
                ! Make an educated guess about which of the three known
                ! node'es (the first, the last, or GUESS) is nearest.
                best_guess = node_nth(e, 1)
                distance = abs(pos - mpos(e, best_guess))

                if (present(guess)) then
                    if (guess /= 0) then
                        d = abs(pos - mpos(e, guess))

                        if (d < distance) then
                            distance = d
                            best_guess = guess
                        end if
                    end if
                end if

                g = node_nth(e, -1)        ! Check the last elem
                d = abs(pos - mpos(e, g))

                if (d < distance) then
                    distance = d
                    best_guess = g
                end if

                if (e%last_node /= 0) then  ! Check "previous".
                    g = e%last_node
                    d = abs(pos - mpos(e, g))

                    if (d < distance) then
                        distance = d
                        best_guess = g
                    end if
                end if

                ! best-guess is now a "best guess".
                ! Find the correct node. First determine in which direction
                ! it lies, and then move in that direction until it is found.
                if (pos >= mpos(e, best_guess)) then
                    ! Loop until we are exactly one node too far down...
                    do while (pos >= mpos(e, best_guess))
                        best_guess = node_next(e, best_guess)
                        if (best_guess == 0) best_guess = DLL
                        if (best_guess == DLL) exit
                    end do

                    ! ...and return the previous node.
                    node = node_prev(e, best_guess)
                else
                    ! Pos is before best-guess
                    do while (pos < mpos(e, best_guess))
                        best_guess = node_prev(e, best_guess)
                        if (best_guess == 0) exit
                    end do

                    node = best_guess
                end if
            end if
        end associate

        call leave_buffer(old)
    end function ewoc_locate

    !> Call EWOC's pretty-printer for each element in NODES.
    !> Delete current text first, thus effecting a "refresh".  Port of
    !> ewoc.el:ewoc-invalidate.
    recursive subroutine ewoc_invalidate(ewoc, nodes)
        type(ewoc_type), intent(in) :: ewoc
        integer, intent(in) :: nodes(:)
        type(handle_type) :: old
        integer :: i, k

        i = slot(ewoc)
        if (i == 0) return
        call enter_buffer(i, old)

        do k = 1, size(nodes)
            if (nodes(k) /= 0) call refresh_node(i, nodes(k))
        end do

        call leave_buffer(old)
    end subroutine ewoc_invalidate

    !> Move point to the ARGth previous element in EWOC.
    !> Don't move if we are at the first element, or if EWOC is empty.
    !> Return the node we moved to.  Port of ewoc.el:ewoc-goto-prev.
    integer function ewoc_goto_prev(ewoc, arg_in) result(node)
        type(ewoc_type), intent(in) :: ewoc
        integer, intent(in) :: arg_in
        type(handle_type) :: old
        integer :: i, arg

        node = 0
        i = slot(ewoc)
        if (i == 0) return
        arg = arg_in
        node = ewoc_locate(ewoc, point())
        call enter_buffer(i, old)

        if (node /= 0) then
            ! If we were past the last element, first jump to it.
            if (point() >= mpos(ewocs(i), ewocs(i)%nodes(node)%right)) arg = arg - 1

            do while (node /= 0 .and. arg > 0)
                arg = arg - 1
                node = node_prev(ewocs(i), node)
            end do

            ! Never step above the first element.
            if (filter_hf_nodes(ewocs(i), node) == 0) node = node_nth(ewocs(i), 1)
            call leave_buffer(old)
            call ewoc_goto_node(ewoc, node)
        else
            call leave_buffer(old)
        end if
    end function ewoc_goto_prev

    !> Move point to the ARGth next element in EWOC.
    !> Return the node (or nil if we just passed the last node).  Port of
    !> ewoc.el:ewoc-goto-next.
    integer function ewoc_goto_next(ewoc, arg_in, status) result(node)
        type(ewoc_type), intent(in) :: ewoc
        integer, intent(in) :: arg_in
        type(status_type), intent(out) :: status
        integer :: i, arg

        status = status_type()
        node = 0
        i = slot(ewoc)
        if (i == 0) return
        arg = arg_in
        node = ewoc_locate(ewoc, point())

        do while (node /= 0 .and. arg > 0)
            arg = arg - 1
            node = node_next(ewocs(i), node)
        end do

        ! Never step below the first element.
        ! (unless (ewoc--filter-hf-nodes ewoc node)
        !   (setq node (ewoc--node-nth dll -2)))
        if (node == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No next")
            return
        end if

        call ewoc_goto_node(ewoc, node)
    end function ewoc_goto_next

    !> Move point to NODE in EWOC.  Port of ewoc.el:ewoc-goto-node.
    subroutine ewoc_goto_node(ewoc, node)
        use simple_fm, only: goal_column
        use indent_fm, only: move_to_column
        type(ewoc_type), intent(in) :: ewoc
        integer, intent(in) :: node
        type(handle_type) :: old
        integer(int64) :: reached
        type(status_type) :: st
        integer :: i

        i = slot(ewoc)
        if (i == 0 .or. node == 0) return
        call enter_buffer(i, old)
        call goto_char(mpos(ewocs(i), node))
        if (goal_column >= 0) call move_to_column(goal_column, reached, .false., st)
        ewocs(i)%last_node = node
        call leave_buffer(old)
    end subroutine ewoc_goto_node

    !> Refresh all data in EWOC.
    !> The pretty-printer that was specified when the EWOC was created
    !> will be called for all elements in EWOC.
    !> Note that `ewoc_invalidate' is more efficient if only a small
    !> number of elements needs to be refreshed.  Port of
    !> ewoc.el:ewoc-refresh.
    recursive subroutine ewoc_refresh(ewoc)
        type(ewoc_type), intent(in) :: ewoc
        type(handle_type) :: old
        type(value_type) :: saved
        type(status_type) :: st
        integer :: i, footer, node

        i = slot(ewoc)
        if (i == 0) return
        call enter_buffer(i, old)
        footer = ewocs(i)%footer
        call symbol_value(sym("inhibit_read_only"), saved, st)
        if (.not. st%ok()) saved = nil()
        call set(sym("inhibit_read_only"), t_value(), st)
        call delete_region(mpos(ewocs(i), node_nth(ewocs(i), 1)), mpos(ewocs(i), footer), st)
        call goto_char(mpos(ewocs(i), footer))
        node = node_nth(ewocs(i), 1)

        do while (node /= footer .and. node /= 0)
            call set_marker(ewocs(i)%nodes(node)%start_marker, point(), st)
            call print_node(ewocs(i), node)
            node = node_next(ewocs(i), node)
        end do

        call set(sym("inhibit_read_only"), saved, st)
        call set_marker(ewocs(i)%nodes(footer)%start_marker, point(), st)
        call leave_buffer(old)
    end subroutine ewoc_refresh

    !> Select elements from EWOC using PREDICATE.
    !> Return a list of all selected data elements.
    !> PREDICATE is a function that takes a data element as its first
    !> argument.  The elements on the returned list will appear in the
    !> same order as in the buffer.  You should not rely on the order of
    !> calls to PREDICATE.
    !> Note that the buffer the EWOC is displayed in is the current
    !> buffer when PREDICATE is called.  PREDICATE must restore it if it
    !> changes it.  Port of ewoc.el:ewoc-collect.
    recursive subroutine ewoc_collect(ewoc, predicate, result)
        type(ewoc_type), intent(in) :: ewoc
        procedure(ewoc_predicate_proc) :: predicate
        integer(int64), allocatable, intent(out) :: result(:)
        integer(int64), allocatable :: found(:)
        type(handle_type) :: old
        integer :: i, node, n

        allocate (result(0))
        i = slot(ewoc)
        if (i == 0) return
        call enter_buffer(i, old)
        allocate (found(ewocs(i)%n))
        n = 0
        node = node_nth(ewocs(i), -2)

        do while (node /= ewocs(i)%header .and. node /= 0)
            if (predicate(ewocs(i)%nodes(node)%data)) then
                n = n + 1
                found(n) = ewocs(i)%nodes(node)%data
            end if

            node = node_prev(ewocs(i), node)
        end do

        ! (push ...): the first found ends last.
        result = found(n:1:-1)
        call leave_buffer(old)
    end subroutine ewoc_collect

    !> Return the buffer that is associated with EWOC.
    !> Return nil (the null handle) if the buffer has been deleted.  Port of
    !> ewoc.el:ewoc-buffer.
    function ewoc_buffer(ewoc) result(buf)
        type(ewoc_type), intent(in) :: ewoc
        type(handle_type) :: buf
        integer :: i

        buf = null_handle()
        i = slot(ewoc)
        if (i == 0) return
        if (is_buffer_live(ewocs(i)%buffer)) buf = ewocs(i)%buffer
    end function ewoc_buffer

    !> Return the HEADER and FOOTER of EWOC.  Port of ewoc.el:ewoc-get-hf.
    subroutine ewoc_get_hf(ewoc, header, footer)
        type(ewoc_type), intent(in) :: ewoc
        character(len=:), allocatable, intent(out) :: header, footer
        integer :: i

        header = ""
        footer = ""
        i = slot(ewoc)
        if (i == 0) return
        header = text_of(ewocs(i)%nodes(ewocs(i)%header)%hf)
        footer = text_of(ewocs(i)%nodes(ewocs(i)%footer)%hf)

    contains

        function text_of(parts) result(text)
            type(ewoc_part_type), intent(in) :: parts(:)
            character(len=:), allocatable :: text
            integer :: k

            text = ""

            do k = 1, size(parts)
                text = text//parts(k)%text
            end do
        end function text_of
    end subroutine ewoc_get_hf

    !> Set the HEADER and FOOTER of EWOC.  Port of ewoc.el:ewoc-set-hf.
    recursive subroutine ewoc_set_hf(ewoc, header, footer)
        type(ewoc_type), intent(in) :: ewoc
        character(len=*), intent(in) :: header, footer
        type(handle_type) :: old
        integer :: i

        i = slot(ewoc)
        if (i == 0) return
        call enter_buffer(i, old)
        call set_text(ewocs(i)%nodes(ewocs(i)%header)%hf, header)
        call set_text(ewocs(i)%nodes(ewocs(i)%footer)%hf, footer)
        call refresh_node(i, ewocs(i)%header)
        call refresh_node(i, ewocs(i)%footer)
        call leave_buffer(old)

    contains

        subroutine set_text(parts, text)
            type(ewoc_part_type), allocatable, intent(inout) :: parts(:)
            character(len=*), intent(in) :: text

            if (allocated(parts)) deallocate (parts)
            allocate (parts(1))
            parts(1)%text = text
            parts(1)%face = ""
        end subroutine set_text
    end subroutine ewoc_set_hf

    !> Set the HEADER and FOOTER of EWOC, made of parts with faces (as
    !> `propertize'd strings are).  Port of ewoc.el:ewoc-set-hf.
    recursive subroutine ewoc_set_hf_parts(ewoc, header, footer)
        type(ewoc_type), intent(in) :: ewoc
        type(ewoc_part_type), intent(in) :: header(:), footer(:)
        type(handle_type) :: old
        integer :: i

        i = slot(ewoc)
        if (i == 0) return
        call enter_buffer(i, old)
        ewocs(i)%nodes(ewocs(i)%header)%hf = header
        ewocs(i)%nodes(ewocs(i)%footer)%hf = footer
        call refresh_node(i, ewocs(i)%header)
        call refresh_node(i, ewocs(i)%footer)
        call leave_buffer(old)
    end subroutine ewoc_set_hf_parts
end module ewoc_fm
