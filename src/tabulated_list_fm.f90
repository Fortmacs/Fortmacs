! tabulated_list_fm.f90 -- generic major mode for tabulated lists.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/tabulated-list.el.
!
! Copyright (C) 2011-2026 Free Software Foundation, Inc.
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

!> Tabulated List mode is a generic major mode for browsing a list of
!> items, divided into columns labeled in the header line.  A mode
!> derived from it sets the current buffer's format
!> (`set_tabulated_list_format': an array of `tabulated_list_column_type'),
!> its entries (`set_tabulated_list_entries': an array of
!> `tabulated_list_entry_type', or a function that makes them), maybe its
!> padding and sort key, calls `tabulated_list_init_header' and then
!> `tabulated_list_print'.
!>
!> Each entry has an ID, a string identifying it (upstream: any Lisp
!> object, compared with `equal'), and one column descriptor (a
!> `tabulated_list_desc_type': a string, maybe with a face) per column.
!> A column sorts by its descriptors' strings (SORT true) or by a
!> TEST on two entries.
!>
!> Deferred: the fake header for `tabulated_list_use_header_line' false,
!> image and button column descriptors and `button_buffer_map' in the
!> mode's keymap, the sort buttons of the header line (mouse clicks, and
!> `tabulated_list_col_sort'), groups (`tabulated_list_groups' and its
!> functions), `truncate_lines' (not ported: long lines wrap), the
!> help-echo and mouse-face of columns, `header_line_indent_mode' (there
!> are no line numbers), bidi.
module tabulated_list_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use callint_fm, only: PREFIX_NONE
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_list, xstring, xfixnum, string_intervals, vtag, V_INTEGER, V_STRING
    use data_fm, only: append_locals_clearer
    use eval_fm, only: hook_type
    use modes_fm, only: major_modes
    use commands_fm, only: commands
    use buffer_fm, only: buffers, current, buffer_serial_live, erase_buffer, restore_buffer_modified
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, point_min, point_max, pos_bol, pos_eol, delete_region, is_eob
    use cmds_fm, only: forward_line, beginning_of_line, forward_char
    use indent_fm, only: current_column, move_to_column
    use textprop_fm, only: get_text_property, put_text_property, add_text_properties, next_single_property_change, &
                           previous_single_property_change, propertize
    use files_fm, only: revert_buffer_function
    use fill_fm, only: string_width
    use frame_fm, only: selected_frame
    use derived_fm, only: is_derived_mode
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use mule_util_fm, only: truncate_string_to_width, truncate_string_to_width_propertized
    use faces_fm, only: named_face_type, face_attrs, display_when
    use keymap_fm, only: keymap_set
    implicit none(type, external)
    private

    public :: tabulated_list_mode, set_tabulated_list_format, tabulated_list_format, set_tabulated_list_entries, &
              set_tabulated_list_entries_function, tabulated_list_entries, set_tabulated_list_padding, &
              tabulated_list_padding, set_tabulated_list_sort_key, clear_tabulated_list_sort_key, &
              tabulated_list_sort_key, set_tabulated_list_use_header_line, set_tabulated_list_printer, &
              tabulated_list_get_id, tabulated_list_get_entry, tabulated_list_put_tag, tabulated_list_clear_all_tags, &
              tabulated_list_init_header, tabulated_list_revert, tabulated_list_print, tabulated_list_print_entry, &
              tabulated_list_print_col, tabulated_list_delete_entry, tabulated_list_set_col, tabulated_list_sort, &
              tabulated_list_widen_current_column, tabulated_list_narrow_current_column, tabulated_list_next_column, &
              tabulated_list_previous_column, syms_of_tabulated_list

    !> A column of the format: its NAME (the label in the header line;
    !> different columns must have different names) and the WIDTH to
    !> reserve for it (for the final column, its value is ignored).  SORT
    !> true means it can sort the entries: by its descriptors' strings, or
    !> by TEST if that is associated.  RIGHT_ALIGN true means the
    !> column is right-aligned; PAD_RIGHT is the number of additional
    !> padding spaces to its right.  Port of an element (NAME WIDTH SORT .
    !> PROPS) of tabulated-list.el tabulated-list-format.
    type, public :: tabulated_list_column_type
        character(len=:), allocatable :: name
        integer :: width = 0
        logical :: sort = .false.
        procedure(tabulated_list_test_procedure), pointer, nopass :: test => null()
        logical :: right_align = .false.
        integer :: pad_right = 1
    end type tabulated_list_column_type

    !> A column descriptor: the string TEXT, printed as-is (it must not
    !> contain newlines), in FACE if that is not empty (a `face'
    !> property).
    type, public :: tabulated_list_desc_type
        character(len=:), allocatable :: text
        character(len=:), allocatable :: face
    end type tabulated_list_desc_type

    !> An entry: its ID, which keeps the cursor on the "same" entry when
    !> rearranging the list (empty for none), and one descriptor per column.
    !> Port of an element (ID [DESC1 ... DESCN]) of tabulated-list.el
    !> tabulated-list-entries.
    type, public :: tabulated_list_entry_type
        character(len=:), allocatable :: id
        type(tabulated_list_desc_type), allocatable :: cols(:)
        !> Its place in the order the list was first displayed in (0 until
        !> the list is sorted by a column; tabulated-list--original-order).
        integer :: original_index = 0
    end type tabulated_list_entry_type

    abstract interface
        !> A SORT test: whether entry A sorts before entry B.
        !> Not a port.
        logical function tabulated_list_test_procedure(a, b)
            import :: tabulated_list_entry_type
            implicit none(type, external)
            type(tabulated_list_entry_type), intent(in) :: a, b
        end function tabulated_list_test_procedure

        !> A function returning the entries (tabulated-list-entries as a
        !> function).  Not a port.
        recursive subroutine tabulated_list_entries_procedure(entries, status)
            import :: tabulated_list_entry_type, status_type
            implicit none(type, external)
            type(tabulated_list_entry_type), allocatable, intent(out) :: entries(:)
            type(status_type), intent(out) :: status
        end subroutine tabulated_list_entries_procedure

        !> A `tabulated_list_printer': insert ENTRY at point.  Not a port.
        recursive subroutine tabulated_list_printer_procedure(entry, status)
            import :: tabulated_list_entry_type, status_type
            implicit none(type, external)
            type(tabulated_list_entry_type), intent(in) :: entry
            type(status_type), intent(out) :: status
        end subroutine tabulated_list_printer_procedure
    end interface

    !> Indicator for columns sorted in ascending order, for GUI frames.
    !> See `tabulated_list_tty_sort_indicator_asc' for the indicator used
    !> on text-mode frames.  Port of tabulated-list.el
    !> tabulated-list-gui-sort-indicator-asc.
    character(len=:), allocatable, public :: tabulated_list_gui_sort_indicator_asc

    !> Indicator for columns sorted in descending order, for GUI frames.
    !> See `tabulated_list_tty_sort_indicator_desc' for the indicator used
    !> on text-mode frames.  Port of tabulated-list.el
    !> tabulated-list-gui-sort-indicator-desc.
    character(len=:), allocatable, public :: tabulated_list_gui_sort_indicator_desc

    !> Indicator for columns sorted in ascending order, for text-mode
    !> frames.  See `tabulated_list_gui_sort_indicator_asc' for the
    !> indicator used on GUI frames.  Port of tabulated-list.el
    !> tabulated-list-tty-sort-indicator-asc.
    character(len=:), allocatable, public :: tabulated_list_tty_sort_indicator_asc

    !> Indicator for columns sorted in ascending order, for text-mode
    !> frames.  See `tabulated_list_gui_sort_indicator_asc' for the
    !> indicator used on GUI frames.  Port of tabulated-list.el
    !> tabulated-list-tty-sort-indicator-desc.
    character(len=:), allocatable, public :: tabulated_list_tty_sort_indicator_desc

    !> Face used on fake header lines.  Port of tabulated-list.el
    !> tabulated-list-fake-header.
    type(named_face_type), target, public :: tabulated_list_fake_header

    !> Hook run before reverting a Tabulated List buffer.
    !> This is commonly used to recompute `tabulated_list_entries'.
    !> Port of tabulated-list.el tabulated-list-revert-hook.
    type(hook_type), target, public :: tabulated_list_revert_hook = hook_type(name="tabulated_list_revert_hook")

    !> Hook run when entering Tabulated List mode.
    type(hook_type), target, public :: tabulated_list_mode_hook = hook_type(name="tabulated_list_mode_hook")

    !> Local keymap for `tabulated_list_mode' buffers.  Port of
    !> tabulated-list.el tabulated-list-mode-map.
    type(handle_type), public :: tabulated_list_mode_map

    !> The buffer-local variables of a Tabulated List buffer.  FORMAT,
    !> ENTRIES, PADDING and the sort key are permanent-local, so that
    !> switching to a different major mode and back keeps them.
    type :: local_type
        integer(int64) :: serial = 0
        !> tabulated-list-format.
        type(tabulated_list_column_type), allocatable :: format(:)
        !> tabulated-list-entries (a list), or ENTRIES_FUNCTION.
        type(tabulated_list_entry_type), allocatable :: entries(:)
        procedure(tabulated_list_entries_procedure), pointer, nopass :: entries_function => null()
        !> tabulated-list-padding.
        integer :: padding = 0
        !> tabulated-list-sort-key: NAME and FLIP, none when SORT_NAME is
        !> unallocated.
        character(len=:), allocatable :: sort_name
        logical :: sort_flip = .false.
        !> tabulated-list-use-header-line.
        logical :: use_header_line = .true.
        !> tabulated-list-printer (null for `tabulated_list_print_entry').
        procedure(tabulated_list_printer_procedure), pointer, nopass :: printer => null()
        !> Whether tabulated-list--original-order is set.
        logical :: original_order = .false.
    end type local_type

    type(local_type), allocatable, target :: locals(:)

    !> tabulated-list--near-rows: the descriptors of the previous, current
    !> and next rows while printing (for the available space of a column
    !> before a right-aligned one).
    type(tabulated_list_entry_type) :: near_rows(3)
    logical :: has_near_rows = .false.

contains

    ! ------------------------------------------------------------------
    ! Buffer-local variables
    ! ------------------------------------------------------------------

    ! The index of the current buffer's entry in `locals', made if MAKE
    ! (0 if there is none).  Not a port.
    integer function local_index(make) result(k)
        logical, intent(in) :: make
        type(local_type), allocatable :: grown(:)
        integer :: i, n

        k = 0
        if (current == 0) return
        if (.not. allocated(locals)) allocate (locals(0))

        do k = 1, size(locals)
            if (locals(k)%serial == buffers(current)%serial) return
        end do

        k = 0
        if (.not. make) return

        do k = 1, size(locals)
            if (.not. buffer_serial_live(locals(k)%serial)) then
                locals(k) = local_type(serial=buffers(current)%serial)
                return
            end if
        end do

        n = size(locals)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i) = locals(i)
        end do

        grown(n + 1)%serial = buffers(current)%serial
        call move_alloc(grown, locals)
        k = n + 1
    end function local_index

    ! kill-all-local-variables: forget the current buffer's values but
    ! the permanent-local ones.  Not a port.
    subroutine clear_tabulated_list_locals()
        integer :: k

        k = local_index(.false.)
        if (k == 0) return
        locals(k)%use_header_line = .true.
        locals(k)%printer => null()
        locals(k)%original_order = .false.
    end subroutine clear_tabulated_list_locals

    !> Set the current buffer's `tabulated_list_format' to COLUMNS, the
    !> format of the current Tabulated List mode buffer.  Port of
    !> tabulated-list.el tabulated-list-format (set_local).
    subroutine set_tabulated_list_format(columns)
        type(tabulated_list_column_type), intent(in) :: columns(:)
        integer :: k, j

        k = local_index(.true.)
        if (allocated(locals(k)%format)) deallocate (locals(k)%format)
        allocate (locals(k)%format(size(columns)))

        do j = 1, size(columns)
            locals(k)%format(j) = columns(j)
        end do
    end subroutine set_tabulated_list_format

    !> The current buffer's `tabulated_list_format' (empty when unset).
    !> Not a port.
    function tabulated_list_format() result(columns)
        type(tabulated_list_column_type), allocatable :: columns(:)
        integer :: k, j

        k = local_index(.false.)

        if (k == 0) then
            allocate (columns(0))
            return
        end if

        if (.not. allocated(locals(k)%format)) allocate (locals(k)%format(0))
        allocate (columns(size(locals(k)%format)))

        do j = 1, size(columns)
            columns(j) = locals(k)%format(j)
        end do
    end function tabulated_list_format

    !> Set the current buffer's `tabulated_list_entries' to ENTRIES, the
    !> entries displayed in the current Tabulated List buffer.  Port of
    !> tabulated-list.el tabulated-list-entries (an array; set_local).
    subroutine set_tabulated_list_entries(entries)
        type(tabulated_list_entry_type), intent(in) :: entries(:)
        integer :: k

        k = local_index(.true.)
        call copy_entries(entries, locals(k)%entries)
        locals(k)%entries_function => null()
    end subroutine set_tabulated_list_entries

    !> Make FN the current buffer's `tabulated_list_entries': it is called
    !> to make the entries.  Port of tabulated-list.el
    !> tabulated-list-entries (a function).
    subroutine set_tabulated_list_entries_function(fn)
        procedure(tabulated_list_entries_procedure) :: fn
        integer :: k

        k = local_index(.true.)
        locals(k)%entries_function => fn
    end subroutine set_tabulated_list_entries_function

    !> The current buffer's entries as last printed (or set).  Not a port.
    function tabulated_list_entries() result(entries)
        type(tabulated_list_entry_type), allocatable :: entries(:)
        integer :: k

        k = local_index(.false.)

        if (k == 0) then
            allocate (entries(0))
            return
        end if

        if (.not. allocated(locals(k)%entries)) allocate (locals(k)%entries(0))
        call copy_entries(locals(k)%entries, entries)
    end function tabulated_list_entries

    ! Not a port.
    subroutine copy_entries(from, to)
        type(tabulated_list_entry_type), intent(in) :: from(:)
        type(tabulated_list_entry_type), allocatable, intent(out) :: to(:)
        integer :: j

        allocate (to(size(from)))

        do j = 1, size(from)
            to(j) = from(j)
        end do
    end subroutine copy_entries

    !> Set the current buffer's `tabulated_list_padding' to N: the number
    !> of characters preceding each Tabulated List mode entry.  By default,
    !> lines are padded with spaces, but you can use the function
    !> `tabulated_list_put_tag' to change this.  Port of tabulated-list.el
    !> tabulated-list-padding (set_local).
    subroutine set_tabulated_list_padding(n)
        integer, intent(in) :: n
        integer :: k

        k = local_index(.true.)
        locals(k)%padding = n
    end subroutine set_tabulated_list_padding

    !> The current buffer's `tabulated_list_padding'.  Not a port.
    integer function tabulated_list_padding() result(n)
        integer :: k

        n = 0
        k = local_index(.false.)
        if (k > 0) n = locals(k)%padding
    end function tabulated_list_padding

    !> Set the current buffer's `tabulated_list_sort_key' to (NAME .
    !> FLIP): NAME is one of the column names in `tabulated_list_format'
    !> (the corresponding SORT entry then specifies how to sort); FLIP
    !> true means to invert the resulting sort.  Port of tabulated-list.el
    !> tabulated-list-sort-key (set_local).
    subroutine set_tabulated_list_sort_key(name, flip)
        character(len=*), intent(in) :: name
        logical, intent(in), optional :: flip
        integer :: k

        k = local_index(.true.)
        locals(k)%sort_name = name
        locals(k)%sort_flip = .false.
        if (present(flip)) locals(k)%sort_flip = flip
    end subroutine set_tabulated_list_sort_key

    !> Make the current buffer's `tabulated_list_sort_key' empty: no
    !> additional sorting is performed.  Not a port.
    subroutine clear_tabulated_list_sort_key()
        integer :: k

        k = local_index(.false.)
        if (k == 0) return
        if (allocated(locals(k)%sort_name)) deallocate (locals(k)%sort_name)
        locals(k)%sort_flip = .false.
    end subroutine clear_tabulated_list_sort_key

    !> The current buffer's `tabulated_list_sort_key': its NAME (empty for
    !> none) and FLIP.  Not a port.
    subroutine tabulated_list_sort_key(name, flip)
        character(len=:), allocatable, intent(out) :: name
        logical, intent(out) :: flip
        integer :: k

        name = ""
        flip = .false.
        k = local_index(.false.)
        if (k == 0) return
        if (allocated(locals(k)%sort_name)) name = locals(k)%sort_name
        flip = locals(k)%sort_flip
    end subroutine tabulated_list_sort_key

    !> Set the current buffer's `tabulated_list_use_header_line': whether
    !> the Tabulated List buffer should use a header line.  Port of
    !> tabulated-list.el tabulated-list-use-header-line (false, the fake
    !> header, is Deferred).
    subroutine set_tabulated_list_use_header_line(flag)
        logical, intent(in) :: flag
        integer :: k

        k = local_index(.true.)
        locals(k)%use_header_line = flag
    end subroutine set_tabulated_list_use_header_line

    !> Make FN the current buffer's `tabulated_list_printer': the function
    !> for inserting a Tabulated List entry at point.  Port of
    !> tabulated-list.el tabulated-list-printer (set_local).
    subroutine set_tabulated_list_printer(fn)
        procedure(tabulated_list_printer_procedure) :: fn
        integer :: k

        k = local_index(.true.)
        locals(k)%printer => fn
    end subroutine set_tabulated_list_printer

    ! ------------------------------------------------------------------
    ! Entries at positions
    ! ------------------------------------------------------------------

    !> Return the entry ID of the Tabulated List entry at POS ("" for
    !> none).  POS, if absent, defaults to point.  Port of
    !> tabulated-list.el:tabulated-list-get-id.
    function tabulated_list_get_id(pos) result(id)
        integer(int64), intent(in), optional :: pos
        character(len=:), allocatable :: id
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: p

        id = ""
        p = point()
        if (present(pos)) p = pos
        if (p < point_min() .or. p >= point_max()) return
        call get_text_property(p, value_type(name="tabulated_list_id"), v, st)
        if (st%ok() .and. vtag(v) == V_STRING) id = xstring(v)
    end function tabulated_list_get_id

    !> Return in ENTRY the Tabulated List entry at POS; FOUND false if
    !> there is no entry at POS.  POS, if absent, defaults to point.
    !> Port of tabulated-list.el:tabulated-list-get-entry.
    subroutine tabulated_list_get_entry(entry, found, pos)
        type(tabulated_list_entry_type), intent(out) :: entry
        logical, intent(out) :: found
        integer(int64), intent(in), optional :: pos
        integer :: k, i

        k = entry_index(pos)
        found = k > 0
        if (.not. found) return
        i = local_index(.false.)
        entry = locals(i)%entries(k)
    end subroutine tabulated_list_get_entry

    ! The index in the current buffer's entries of the entry at POS
    ! (point if absent), 0 for none.  Not a port.
    integer function entry_index(pos) result(k)
        integer(int64), intent(in), optional :: pos
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: p
        integer :: i

        k = 0
        p = point()
        if (present(pos)) p = pos
        if (p < point_min() .or. p >= point_max()) return
        call get_text_property(p, value_type(name="tabulated_list_entry"), v, st)
        if (.not. st%ok() .or. vtag(v) /= V_INTEGER) return
        i = local_index(.false.)
        if (i == 0) return
        if (.not. allocated(locals(i)%entries)) return
        k = int(xfixnum(v))
        if (k < 1 .or. k > size(locals(i)%entries)) k = 0
    end function entry_index

    !> Put TAG in the padding area of the current line.
    !> TAG should be a string, with length <= `tabulated_list_padding'.
    !> If ADVANCE is true, move forward by one line afterwards.  Port of
    !> tabulated-list.el:tabulated-list-put-tag.
    recursive subroutine tabulated_list_put_tag(tag, status, advance)
        character(len=*), intent(in) :: tag
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: advance
        integer(int64) :: saved, beginning, shortage, width
        integer :: padding
        logical :: ro

        status = status_type()
        padding = tabulated_list_padding()

        if (.not. (padding > 0)) then
            call set_error(status, FM_GENERIC_ERROR, "Unable to tag the current line")
            return
        end if

        saved = point()
        call beginning_of_line()

        if (entry_index() > 0) then
            beginning = point()
            ro = buffers(current)%read_only
            buffers(current)%read_only = .false.
            call goto_char(beginning + padding)
            width = string_width(tag)

            ! (insert-and-inherit: the line's properties.)
            if (width <= padding) then
                call insert_inheriting(tag//repeat(" ", int(padding - width)), status)
            else
                call insert_inheriting(truncate_string_to_width(tag, padding), status)
            end if

            if (status%ok()) call delete_region(beginning, beginning + padding, status)
            buffers(current)%read_only = ro
        end if

        call goto_char(saved)

        if (present(advance)) then
            if (advance) shortage = forward_line()
        end if
    end subroutine tabulated_list_put_tag

    ! Insert TEXT at point with the text properties of the character
    ! after it (as insert-and-inherit takes those of the line's entry).
    ! Not a port.
    recursive subroutine insert_inheriting(text, status)
        use textprop_fm, only: text_properties_at
        character(len=*), intent(in) :: text
        type(status_type), intent(out) :: status
        type(value_type) :: plist
        integer(int64) :: beginning
        logical :: changed

        beginning = point()
        call text_properties_at(beginning, plist, status)
        if (.not. status%ok()) return
        call insert(text, status)
        if (.not. status%ok()) return
        if (allocated(plist%a)) call add_text_properties(beginning, point(), plist, changed, status)
    end subroutine insert_inheriting

    !> Clear all tags from the padding area in the current buffer.  Port of
    !> tabulated-list.el:tabulated-list-clear-all-tags.
    recursive subroutine tabulated_list_clear_all_tags(status)
        use search_fm, only: re_search_forward
        type(status_type), intent(out) :: status
        integer(int64) :: saved, found
        integer :: padding
        character(len=24) :: buffer
        logical :: ro

        status = status_type()
        padding = tabulated_list_padding()

        if (.not. (padding > 0)) then
            call set_error(status, FM_GENERIC_ERROR, "There can be no tags in current buffer")
            return
        end if

        saved = point()
        call goto_char(point_min())
        ro = buffers(current)%read_only
        buffers(current)%read_only = .false.
        ! Match non-space in the first n characters.
        write (buffer, "(i0)") padding - 1

        do
            call re_search_forward("^ \{0,"//trim(buffer)//"\}[^ ]", found, status, noerror=1)
            if (.not. status%ok() .or. found == 0) exit
            call tabulated_list_put_tag(repeat(" ", padding), status)
            if (.not. status%ok()) exit
        end do

        buffers(current)%read_only = ro
        call goto_char(saved)
    end subroutine tabulated_list_clear_all_tags

    ! ------------------------------------------------------------------
    ! The header
    ! ------------------------------------------------------------------

    !> Set up header line for the Tabulated List buffer.  Port of
    !> tabulated-list.el:tabulated-list-init-header (the header line's
    !> segments; the sort indicator of a text-mode frame is the one its
    !> glyphless-char-display table shows upstream).
    subroutine tabulated_list_init_header()
        use buffer_fm, only: header_line_format_type
        type(header_line_format_type) :: header
        type(tabulated_list_column_type), allocatable :: format(:)
        character(len=:), allocatable :: label, pname, sort_name, indicator
        integer :: x, n, len_fmt, lablen, next_x, available_space, shift, width, k
        logical :: not_last_col, flip

        k = local_index(.true.)
        format = tabulated_list_format()
        call tabulated_list_sort_key(sort_name, flip)
        x = max(locals(k)%padding, 0)
        len_fmt = size(format)
        ! Pre-compute width for available-space computation.
        has_near_rows = .true.
        call header_row(format, near_rows(1))
        call header_row(format, near_rows(2))
        call header_row(format, near_rows(3))
        call header%append("", align_to=x)

        do n = 1, len_fmt
            not_last_col = n < len_fmt
            label = format(n)%name
            lablen = int(string_width(label))
            pname = label
            width = format(n)%width
            next_x = x + format(n)%pad_right + width
            available_space = 0

            if (not_last_col) then
                if (format(n)%right_align) then
                    available_space = width
                else
                    available_space = tabulated_list_available_space(format, width, n)
                end if
            end if

            if (lablen >= 3 .and. not_last_col .and. lablen > available_space) then
                label = truncate_string_to_width(label, available_space, default_ellipsis=.true.)
            end if

            if (.not. format(n)%sort) then
                ! An unsortable column
                call append(label, "")
            else if (same(pname, sort_name)) then
                ! The selected sort column
                indicator = ""

                if (.not. (lablen < 3 .and. not_last_col)) then
                    if (flip) then
                        indicator = " "//desc_indicator()
                    else
                        indicator = " "//asc_indicator()
                    end if
                end if

                call append(label//indicator, "bold")
            else
                ! Unselected sortable column.
                call append(label, "")
            end if

            if (format(n)%pad_right >= 0) then
                ! (The space that aligns the next column.)
                call header%append("", align_to=next_x)
            end if

            x = next_x
        end do

        has_near_rows = .false.

        if (locals(k)%use_header_line) then
            buffers(current)%header_line_format = header
        end if

    contains

        subroutine append(text, face)
            character(len=*), intent(in) :: text, face

            if (format(n)%right_align) then
                shift = width - int(string_width(text))

                if (shift > 0) then
                    call header%append(text, face, align_to=x + shift)
                    x = x + shift
                    return
                end if
            end if

            call header%append(text, face)
        end subroutine append
    end subroutine tabulated_list_init_header

    ! The column names as a row of descriptors (hcols).  Not a port.
    subroutine header_row(format, row)
        type(tabulated_list_column_type), intent(in) :: format(:)
        type(tabulated_list_entry_type), intent(out) :: row
        integer :: j

        allocate (row%cols(size(format)))

        do j = 1, size(format)
            row%cols(j)%text = format(j)%name
            row%cols(j)%face = ""
        end do
    end subroutine header_row

    ! Not a port.
    function asc_indicator() result(s)
        character(len=:), allocatable :: s

        if (associated(selected_frame%rif)) then
            s = tabulated_list_gui_sort_indicator_asc
        else
            s = tabulated_list_tty_sort_indicator_asc
        end if
    end function asc_indicator

    ! Not a port.
    function desc_indicator() result(s)
        character(len=:), allocatable :: s

        if (associated(selected_frame%rif)) then
            s = tabulated_list_gui_sort_indicator_desc
        else
            s = tabulated_list_tty_sort_indicator_desc
        end if
    end function desc_indicator

    ! Not a port.
    pure logical function same(a, b)
        character(len=*), intent(in) :: a, b

        same = len(a) == len(b)
        if (same) same = a == b
    end function same

    !> The `revert_buffer_function' for `tabulated_list_mode'.
    !> It runs `tabulated_list_revert_hook', then calls
    !> `tabulated_list_print'.  Port of
    !> tabulated-list.el:tabulated-list-revert.
    recursive subroutine tabulated_list_revert(status)
        type(status_type), intent(out) :: status

        if (.not. is_derived_mode("tabulated_list_mode")) then
            call set_error(status, FM_GENERIC_ERROR, "The current buffer is not in Tabulated List mode")
            return
        end if

        call tabulated_list_revert_hook%execute(status)
        if (.not. status%ok()) return
        call tabulated_list_print(status, remember_pos=.true.)
    end subroutine tabulated_list_revert

    ! Not a port.
    recursive subroutine revert_function(ignore_auto, noconfirm, status)
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status

        if (ignore_auto .and. noconfirm) continue
        call tabulated_list_revert(status)
    end subroutine revert_function

    ! The index (1-based) of the column named NAME, 0 (and an error) if
    ! there is none.
    ! Port of tabulated-list.el:tabulated-list--column-number.
    integer function column_number(format, name, status) result(n)
        type(tabulated_list_column_type), intent(in) :: format(:)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status

        status = status_type()

        do n = 1, size(format)
            if (same(format(n)%name, name)) return
        end do

        n = 0
        call set_error(status, FM_GENERIC_ERROR, "No column named "//name)
    end function column_number

    ! Whether entry A sorts before entry B by the sort key of local K.
    ! Port of tabulated-list.el:tabulated-list--get-sorter.
    logical function sorts_before(k, n, a, b) result(before)
        integer, intent(in) :: k, n
        type(tabulated_list_entry_type), intent(in) :: a, b

        if (locals(k)%sort_flip) then
            before = sorter(b, a)
        else
            before = sorter(a, b)
        end if

    contains

        logical function sorter(x, y)
            type(tabulated_list_entry_type), intent(in) :: x, y

            if (associated(locals(k)%format(n)%test)) then
                sorter = locals(k)%format(n)%test(x, y)
            else
                ! Default sorter checks column N:
                sorter = string_less(x%cols(n)%text, y%cols(n)%text)
            end if
        end function sorter
    end function sorts_before

    ! string<: by character code (the order of the UTF-8 bytes).
    ! Not a port.
    pure logical function string_less(a, b)
        character(len=*), intent(in) :: a, b
        integer :: k

        do k = 1, min(len(a), len(b))
            if (a(k:k) /= b(k:k)) then
                string_less = iachar(a(k:k)) < iachar(b(k:k))
                return
            end if
        end do

        string_less = len(a) < len(b)
    end function string_less

    ! The sort column of local K (0 for no sorter: no sort key, or an
    ! unsortable column).
    ! Port of part of tabulated-list.el:tabulated-list--get-sorter.
    integer function sort_column(k, status) result(n)
        integer, intent(in) :: k
        type(status_type), intent(out) :: status

        n = 0
        status = status_type()
        if (.not. allocated(locals(k)%sort_name)) return
        if (.not. allocated(locals(k)%format)) return
        n = column_number(locals(k)%format, locals(k)%sort_name, status)

        if (n > 0) then
            if (.not. locals(k)%format(n)%sort) n = 0
        end if
    end function sort_column

    ! Sort ENTRIES stably by column N of local K.  Not a port.
    subroutine sort_entries(k, n, entries)
        integer, intent(in) :: k, n
        type(tabulated_list_entry_type), intent(inout) :: entries(:)
        type(tabulated_list_entry_type) :: t
        integer :: i, j

        do i = 2, size(entries)
            t = entries(i)
            j = i - 1

            do while (j >= 1)
                if (.not. sorts_before(k, n, t, entries(j))) exit
                entries(j + 1) = entries(j)
                j = j - 1
            end do

            entries(j + 1) = t
        end do
    end subroutine sort_entries

    ! ------------------------------------------------------------------
    ! Printing
    ! ------------------------------------------------------------------

    !> Populate the current Tabulated List mode buffer.
    !> This sorts the `tabulated_list_entries' list if sorting is specified
    !> by `tabulated_list_sort_key'.  It then erases the buffer and inserts
    !> the entries with `tabulated_list_printer'.
    !>
    !> Optional argument REMEMBER_POS, if true, means to move point to the
    !> entry with the same ID element as the current line.
    !>
    !> True UPDATE argument means to use an alternative printing method
    !> which is faster if most entries haven't changed since the last
    !> print.  The only difference in outcome is that tags will not be
    !> removed from entries that haven't changed (see
    !> `tabulated_list_put_tag').  Don't use this immediately after changing
    !> `tabulated_list_sort_key'.  Port of
    !> tabulated-list.el:tabulated-list-print (no groups).
    recursive subroutine tabulated_list_print(status, remember_pos, update)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: remember_pos, update
        type(tabulated_list_entry_type), allocatable :: entries(:)
        character(len=:), allocatable :: entry_id
        integer(int64) :: saved_pt, saved_col, reached, shortage
        integer :: k, n, i
        logical :: remember, upd, ro, has_id

        k = local_index(.true.)
        remember = .false.
        if (present(remember_pos)) remember = remember_pos
        upd = .false.
        if (present(update)) upd = update

        if (associated(locals(k)%entries_function)) then
            call locals(k)%entries_function(entries, status)
            if (.not. status%ok()) return
        else
            if (.not. allocated(locals(k)%entries)) allocate (locals(k)%entries(0))
            call copy_entries(locals(k)%entries, entries)
        end if

        if (.not. allocated(locals(k)%format)) allocate (locals(k)%format(0))
        n = sort_column(k, status)
        if (.not. status%ok()) return
        has_id = .false.
        saved_col = 0

        if (remember) then
            entry_id = tabulated_list_get_id()
            has_id = len(entry_id) > 0
            if (has_id) saved_col = current_column()
        end if

        ! Sort the entries, if necessary.
        if (n > 0) call sort_entries(k, n, entries)
        call copy_entries(entries, locals(k)%entries)
        ! Without a sorter, we have no way to just update.
        if (upd .and. n == 0) upd = .false.
        ro = buffers(current)%read_only
        buffers(current)%read_only = .false.

        if (upd) then
            call goto_char(point_min())
        else
            ! Redo the buffer, unless we're just updating.
            call erase_buffer(status)
        end if

        ! Finally, print the resulting list.
        saved_pt = 0

        do i = 1, size(entries)
            call print_one(i)
            if (.not. status%ok()) exit
        end do

        if (upd .and. status%ok()) call delete_region(point(), point_max(), status)
        buffers(current)%read_only = ro
        if (.not. status%ok()) return
        call restore_buffer_modified(.false.)

        ! If REMEMBER_POS was specified, move to the "old" location.
        if (saved_pt > 0) then
            call goto_char(saved_pt)
            call move_to_column(saved_col, reached)
        else
            call goto_char(point_min())
        end if

    contains

        ! Port of the loop of tabulated-list.el:tabulated-list-print-entries.
        recursive subroutine print_one(i)
            integer, intent(in) :: i
            integer :: here
            character(len=:), allocatable :: local_id

            has_near_rows = .true.
            here = entry_index(pos_bol(0_int64))

            if (here > 0 .and. pos_bol(0_int64) < point()) then
                near_rows(1) = locals(k)%entries(here)
            else
                near_rows(1) = entries(i)
            end if

            near_rows(2) = entries(i)

            if (i < size(entries)) then
                near_rows(3) = entries(i + 1)
            else
                near_rows(3) = entries(i)
            end if

            if (has_id) then
                if (same(entry_id, entries(i)%id)) then
                    has_id = .false.
                    saved_pt = point()
                end if
            end if

            ! If the buffer is empty, simply print each elt.
            if (.not. upd .or. is_eob()) then
                call print_with_printer(i)
            else
                do
                    local_id = tabulated_list_get_id()

                    ! If we find id, then nothing to update.
                    if (same(entries(i)%id, local_id) .and. len(local_id) > 0) then
                        shortage = forward_line(1_int64)
                        exit
                    end if

                    ! If this entry sorts after id (or it's the end), then
                    ! just insert id and move on.
                    if (len(local_id) == 0 .or. entry_index() == 0) then
                        call print_with_printer(i)
                        exit
                    end if

                    if (sorts_before(k, n, entries(i), locals(k)%entries(entry_index()))) then
                        call print_with_printer(i)
                        exit
                    end if

                    ! We find an entry that sorts before id, it needs to be
                    ! deleted.
                    block
                        integer(int64) :: old
                        old = point()
                        shortage = forward_line(1_int64)
                        call delete_region(old, point(), status)
                    end block

                    if (.not. status%ok()) exit
                end do
            end if

            has_near_rows = .false.
        end subroutine print_one

        recursive subroutine print_with_printer(i)
            integer, intent(in) :: i
            integer :: before

            before = int(point())

            if (associated(locals(k)%printer)) then
                call locals(k)%printer(entries(i), status)
            else
                call tabulated_list_print_entry(entries(i), status)
            end if

            ! (The entry's index for `tabulated_list_get_entry'.)
            if (status%ok()) call put_text_property(int(before, int64), point(), value_type(name="tabulated_list_entry"), &
                                                    value_type(int(i, int64)), status)
        end subroutine print_with_printer
    end subroutine tabulated_list_print

    !> Insert a Tabulated List ENTRY at point.
    !> This is the default `tabulated_list_printer' function.  Port of
    !> tabulated-list.el:tabulated-list-print-entry.
    recursive subroutine tabulated_list_print_entry(entry, status)
        type(tabulated_list_entry_type), intent(in) :: entry
        type(status_type), intent(out) :: status
        type(tabulated_list_column_type), allocatable :: format(:)
        type(value_type) :: props(2)
        integer(int64) :: beginning
        integer :: x, n, padding
        logical :: ro, saved_near, changed

        beginning = point()
        format = tabulated_list_format()
        padding = tabulated_list_padding()
        x = max(padding, 0)
        ro = buffers(current)%read_only
        buffers(current)%read_only = .false.
        status = status_type()
        if (padding > 0) call insert(repeat(" ", x), status)
        ! Bind tabulated-list--near-rows if not bound yet (Bug#25506).
        saved_near = has_near_rows

        if (.not. has_near_rows) then
            near_rows(1) = entry
            near_rows(2) = entry
            near_rows(3) = entry
            has_near_rows = .true.
        end if

        do n = 1, size(format)
            if (n > size(entry%cols)) exit
            x = tabulated_list_print_col(n, entry%cols(n), x, status)
            if (.not. status%ok()) exit
        end do

        has_near_rows = saved_near
        if (status%ok()) call insert(achar(10), status)

        if (status%ok()) then
            props(1) = value_type(name="tabulated_list_id")
            props(2) = value_type(text=entry%id)
            call add_text_properties(beginning, point(), make_list(props), changed, status)
        end if

        buffers(current)%read_only = ro
    end subroutine tabulated_list_print_entry

    ! The space available for column N of WIDTH: more than WIDTH when the
    ! next column is right-aligned and has room left.  Port of
    ! tabulated-list.el:tabulated-list--available-space.
    integer function tabulated_list_available_space(format, width, n) result(space)
        type(tabulated_list_column_type), intent(in) :: format(:)
        integer, intent(in) :: width, n
        integer :: next_col_width

        if (format(n + 1)%right_align) then
            next_col_width = format(n + 1)%width
            space = width + next_col_width - min(next_col_width, col_local_max_widths(n + 1))
        else
            space = width
        end if
    end function tabulated_list_available_space

    ! Return maximum entry widths at column COL around current row.  Check
    ! the current row, the previous one and the next row.  Port of
    ! tabulated-list.el:tabulated-list--col-local-max-widths.
    integer function col_local_max_widths(col) result(w)
        integer, intent(in) :: col
        integer :: r

        w = 0

        do r = 1, 3
            if (.not. allocated(near_rows(r)%cols)) cycle
            if (col > size(near_rows(r)%cols)) cycle
            w = max(w, int(string_width(near_rows(r)%cols(col)%text)))
        end do
    end function col_local_max_widths

    !> Insert a specified Tabulated List entry at point.
    !> N is the column number, COL_DESC is a column descriptor (see
    !> `tabulated_list_entry_type'), and X is the column number at point.
    !> Return the column number after insertion.  Port of
    !> tabulated-list.el:tabulated-list-print-col (no images or buttons).
    recursive integer function tabulated_list_print_col(n, col_desc, x_in, status) result(next_x)
        integer, intent(in) :: n, x_in
        type(tabulated_list_desc_type), intent(in) :: col_desc
        type(status_type), intent(out) :: status
        type(tabulated_list_column_type), allocatable :: format(:)
        type(value_type) :: label, props(2), spec(3), align(2)
        integer(int64) :: opoint
        integer :: width, pad_right, label_width, available_space, shift, x
        logical :: not_last_col, right_align

        status = status_type()
        format = tabulated_list_format()
        x = x_in
        width = format(n)%width
        pad_right = format(n)%pad_right
        right_align = format(n)%right_align
        label = value_type(text=col_desc%text)
        label_width = int(string_width(col_desc%text))
        opoint = point()
        not_last_col = n < size(format)
        available_space = 0

        if (not_last_col) then
            if (right_align) then
                available_space = width
            else
                available_space = tabulated_list_available_space(format, width, n)
            end if
        end if

        ! Truncate labels if necessary (except last column).  Don't
        ! truncate to `width' if the next column is align-right and has
        ! some space left, truncate to `available_space' instead.
        if (not_last_col .and. label_width > available_space) then
            label = truncate_string_to_width_propertized(col_desc%text, available_space, default_ellipsis=.true.)
            label_width = available_space
        end if

        if (right_align .and. width > label_width) then
            shift = width - label_width
            call insert_spaces(shift, x + shift)
            if (.not. status%ok()) return
            width = width - shift
            x = x + shift
        end if

        if (len(col_desc%face) > 0) then
            props(1) = value_type(name="face")
            props(2) = value_type(name=col_desc%face)
            label = propertize(label, make_list(props))
        end if

        call insert_value(label, status)
        if (.not. status%ok()) return
        next_x = x + pad_right + width

        ! No need to append any spaces if this is the last column.
        if (not_last_col) then
            if (pad_right > 0) call insert(repeat(" ", pad_right), status)
            if (.not. status%ok()) return
            call insert_spaces(width - min(width, label_width), next_x)
            if (.not. status%ok()) return
        end if

        call put_text_property(opoint, point(), value_type(name="tabulated_list_column_name"), value_type(text=format(n)%name), &
                               status)

    contains

        ! Insert COUNT spaces with the display spec (space :align-to
        ! COLUMN).
        recursive subroutine insert_spaces(count, column)
            integer, intent(in) :: count, column
            integer(int64) :: beginning

            beginning = point()
            call insert(repeat(" ", max(0, count)), status)
            if (.not. status%ok() .or. count <= 0) return
            spec(1) = value_type(name="space")
            spec(2) = value_type(name=":align-to")
            spec(3) = value_type(int(column, int64))
            align(1) = value_type(name="display")
            align(2) = make_list(spec)
            call put_text_property(beginning, point(), align(1), align(2), status)
        end subroutine insert_spaces
    end function tabulated_list_print_col

    ! Insert string V at point with its text properties.  Not a port.
    recursive subroutine insert_value(v, status)
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status
        integer(int64), allocatable :: starts(:), ends(:)
        type(value_type), allocatable :: plists(:)
        integer(int64) :: beginning
        integer :: k
        logical :: changed

        beginning = point()
        call insert(xstring(v), status)
        if (.not. status%ok()) return
        call string_intervals(v, starts, ends, plists)

        do k = 1, size(starts)
            if (.not. allocated(plists(k)%a)) cycle
            call add_text_properties(beginning + starts(k), beginning + ends(k), plists(k), changed, status)
            if (.not. status%ok()) return
        end do
    end subroutine insert_value

    !> Delete the Tabulated List entry at point.
    !> Return in ENTRY the deleted entry (its ID and its column
    !> descriptors).  Move point to the beginning of the deleted entry.
    !> FOUND is false if there is no entry at point.
    !>
    !> This function only changes the buffer contents; it does not alter
    !> `tabulated_list_entries'.  Port of
    !> tabulated-list.el:tabulated-list-delete-entry.
    recursive subroutine tabulated_list_delete_entry(entry, found, status)
        type(tabulated_list_entry_type), intent(out) :: entry
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        logical :: ro

        ! Assume that each entry occupies one line.
        status = status_type()
        call tabulated_list_get_entry(entry, found)
        if (.not. found) return
        entry%id = tabulated_list_get_id()
        ro = buffers(current)%read_only
        buffers(current)%read_only = .false.
        call delete_region(pos_bol(), pos_eol() + 1, status)
        buffers(current)%read_only = ro
    end subroutine tabulated_list_delete_entry

    !> Change the Tabulated List entry at point, setting COL to DESC.
    !> COL is the column number to change (1-based), or the name of the
    !> column to change (COL_NAME).  DESC is the new column descriptor,
    !> which is inserted via `tabulated_list_print_col'.
    !>
    !> If CHANGE_ENTRY_DATA is true, modify the underlying entry data by
    !> setting the appropriate slot of the entry originally used to print
    !> this entry.  Port of tabulated-list.el:tabulated-list-set-col.
    recursive subroutine tabulated_list_set_col(desc, status, col, col_name, change_entry_data)
        type(tabulated_list_desc_type), intent(in) :: desc
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: col
        character(len=*), intent(in), optional :: col_name
        logical, intent(in), optional :: change_entry_data
        type(tabulated_list_column_type), allocatable :: format(:)
        type(value_type) :: v, id, entry_prop
        character(len=:), allocatable :: name
        integer(int64) :: opoint, eol, pos, next, here
        integer :: c, k, e, x
        logical :: found, ro, change
        type(status_type) :: st

        opoint = point()
        eol = pos_eol()
        pos = pos_bol()
        format = tabulated_list_format()

        if (present(col)) then
            c = col

            if (c < 1 .or. c > size(format)) then
                call set_error(status, FM_GENERIC_ERROR, "Invalid column")
                return
            end if

            name = format(c)%name
        else if (present(col_name)) then
            name = col_name
            c = column_number(format, col_name, status)
            if (.not. status%ok()) return
        else
            call set_error(status, FM_GENERIC_ERROR, "Invalid column")
            return
        end if

        e = entry_index(pos)

        if (e == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No Tabulated List entry at position")
            return
        end if

        call get_text_property(pos, value_type(name="tabulated_list_id"), id, st)
        call get_text_property(pos, value_type(name="tabulated_list_entry"), entry_prop, st)
        call get_text_property(pos, value_type(name="tabulated_list_column_name"), v, st)
        found = .false.
        if (vtag(v) == V_STRING) found = same(xstring(v), name)

        do while (.not. found)
            call next_single_property_change(pos, value_type(name="tabulated_list_column_name"), next, found, st, eol)
            if (.not. found) next = eol
            pos = next
            found = .false.
            if (pos >= eol) exit
            call get_text_property(pos, value_type(name="tabulated_list_column_name"), v, st)
            if (vtag(v) == V_STRING) found = same(xstring(v), name)
        end do

        status = status_type()
        if (pos >= eol) return
        ro = buffers(current)%read_only
        buffers(current)%read_only = .false.
        call next_single_property_change(pos, value_type(name="tabulated_list_column_name"), next, found, st, eol)
        if (.not. found) next = eol
        call delete_region(pos, next, status)

        if (status%ok()) then
            call goto_char(pos)
            k = local_index(.false.)
            has_near_rows = .true.
            here = entry_index(pos_bol(0_int64))

            if (here > 0 .and. pos_bol(0_int64) < pos_bol()) then
                near_rows(1) = locals(k)%entries(here)
            else
                near_rows(1) = locals(k)%entries(e)
            end if

            near_rows(2) = locals(k)%entries(e)
            here = entry_index(pos_bol(2_int64))

            if (here > 0) then
                near_rows(3) = locals(k)%entries(here)
            else
                near_rows(3) = locals(k)%entries(e)
            end if

            x = tabulated_list_print_col(c, desc, int(current_column()), status)
            has_near_rows = .false.
            change = .false.
            if (present(change_entry_data)) change = change_entry_data
            if (change) locals(k)%entries(e)%cols(c) = desc
            if (status%ok()) call put_text_property(pos, point(), value_type(name="tabulated_list_id"), id, status)
            if (status%ok()) call put_text_property(pos, point(), value_type(name="tabulated_list_entry"), entry_prop, status)
            call goto_char(opoint)
        end if

        buffers(current)%read_only = ro
    end subroutine tabulated_list_set_col

    ! ------------------------------------------------------------------
    ! Sorting and columns
    ! ------------------------------------------------------------------

    !> Sort Tabulated List entries by the column at point.
    !> With a numeric prefix argument N, sort the Nth column.
    !>
    !> If the numeric prefix is -1, restore order the list was originally
    !> displayed in.  Port of tabulated-list.el:tabulated-list-sort (N
    !> counts from 0, as upstream).
    recursive subroutine tabulated_list_sort(status, n)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: n
        type(tabulated_list_column_type), allocatable :: format(:)
        type(value_type) :: v
        character(len=:), allocatable :: name
        type(status_type) :: st
        integer :: k, c

        status = status_type()
        format = tabulated_list_format()

        if (present(n)) then
            if (n >= size(format) .or. n < -1) then
                call set_error(status, FM_USER_ERROR, "Invalid column number")
                return
            end if
        end if

        k = local_index(.true.)

        if (present(n)) then
            if (n == -1) then
                ! Restore original order.
                if (.not. locals(k)%original_order) then
                    call set_error(status, FM_GENERIC_ERROR, "Order is already in original order")
                    return
                end if

                call sort_by_original_index(locals(k)%entries)
                if (allocated(locals(k)%sort_name)) deallocate (locals(k)%sort_name)
                locals(k)%sort_flip = .false.
                call tabulated_list_init_header()
                call tabulated_list_print(status, remember_pos=.true.)
                return
            end if
        end if

        ! Sort based on a column name.
        name = ""

        if (present(n)) then
            name = format(n + 1)%name
        else
            call get_text_property(point(), value_type(name="tabulated_list_column_name"), v, st)
            if (vtag(v) == V_STRING) name = xstring(v)
        end if

        c = 0

        do k = 1, size(format)
            if (same(format(k)%name, name)) c = k
        end do

        if (c > 0) then
            if (format(c)%sort) then
                call sort_by_column_name(name, status)
                return
            end if
        end if

        call set_error(status, FM_USER_ERROR, "Cannot sort by "//name)
    end subroutine tabulated_list_sort

    recursive subroutine cmd_tabulated_list_sort(status)
        type(status_type), intent(out) :: status

        if (current_prefix_arg%kind == PREFIX_NONE) then
            call tabulated_list_sort(status)
        else
            call tabulated_list_sort(status, int(prefix_numeric_value(current_prefix_arg)))
        end if
    end subroutine cmd_tabulated_list_sort

    ! Not a port.
    subroutine sort_by_original_index(entries)
        type(tabulated_list_entry_type), intent(inout) :: entries(:)
        type(tabulated_list_entry_type) :: t
        integer :: i, j

        do i = 2, size(entries)
            t = entries(i)
            j = i - 1

            do while (j >= 1)
                if (.not. t%original_index < entries(j)%original_index) exit
                entries(j + 1) = entries(j)
                j = j - 1
            end do

            entries(j + 1) = t
        end do
    end subroutine sort_by_original_index

    ! Port of tabulated-list.el:tabulated-list--sort-by-column-name.
    recursive subroutine sort_by_column_name(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        integer :: k, i

        status = status_type()
        if (len(name) == 0 .or. .not. is_derived_mode("tabulated_list_mode")) return
        k = local_index(.true.)

        if (.not. locals(k)%original_order) then
            ! Store the original order so that we can restore it later.
            locals(k)%original_order = .true.

            if (allocated(locals(k)%entries)) then
                do i = 1, size(locals(k)%entries)
                    locals(k)%entries(i)%original_index = i
                end do
            end if
        end if

        ! Flip the sort order on a second click.
        if (allocated(locals(k)%sort_name)) then
            if (same(locals(k)%sort_name, name)) then
                locals(k)%sort_flip = .not. locals(k)%sort_flip
            else
                call set_tabulated_list_sort_key(name)
            end if
        else
            call set_tabulated_list_sort_key(name)
        end if

        call tabulated_list_init_header()
        call tabulated_list_print(status, remember_pos=.true.)
    end subroutine sort_by_column_name

    !> Widen the current tabulated-list column by N chars.
    !> Interactively, N is the prefix numeric argument, and defaults to 1.
    !> Port of tabulated-list.el:tabulated-list-widen-current-column.
    recursive subroutine tabulated_list_widen_current_column(status, n)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: n
        type(tabulated_list_entry_type) :: entry
        integer(int64) :: start
        integer :: k, col_nb, total_width, col_width, by
        logical :: found

        status = status_type()
        by = 1
        if (present(n)) by = n
        start = current_column()
        call tabulated_list_get_entry(entry, found)
        if (.not. found) return
        k = local_index(.false.)
        col_nb = 1
        total_width = 0

        do while (col_nb <= size(locals(k)%format))
            col_width = locals(k)%format(col_nb)%width
            total_width = total_width + max(col_width, int(string_width(entry%cols(col_nb)%text))) + &
                          locals(k)%format(col_nb)%pad_right

            if (start >= total_width) then
                col_nb = col_nb + 1
            else
                locals(k)%format(col_nb)%width = max(1, col_width + by)
                call tabulated_list_print(status, remember_pos=.true.)
                call tabulated_list_init_header()
                return
            end if
        end do
    end subroutine tabulated_list_widen_current_column

    recursive subroutine cmd_tabulated_list_widen_current_column(status)
        type(status_type), intent(out) :: status

        call tabulated_list_widen_current_column(status, int(prefix_numeric_value(current_prefix_arg)))
    end subroutine cmd_tabulated_list_widen_current_column

    !> Narrow the current tabulated list column by N chars.
    !> Interactively, N is the prefix numeric argument, and defaults to 1.
    !> Port of tabulated-list.el:tabulated-list-narrow-current-column.
    recursive subroutine tabulated_list_narrow_current_column(status, n)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: n
        integer :: by

        by = 1
        if (present(n)) by = n
        call tabulated_list_widen_current_column(status, -by)
    end subroutine tabulated_list_narrow_current_column

    recursive subroutine cmd_tabulated_list_narrow_current_column(status)
        type(status_type), intent(out) :: status

        call tabulated_list_narrow_current_column(status, int(prefix_numeric_value(current_prefix_arg)))
    end subroutine cmd_tabulated_list_narrow_current_column

    !> Go to the start of the next column after point on the current line.
    !> If ARG is provided, move that many columns.  Port of
    !> tabulated-list.el:tabulated-list-next-column.
    subroutine tabulated_list_next_column(arg)
        integer, intent(in), optional :: arg
        integer(int64) :: next
        type(status_type) :: st
        logical :: found
        integer :: i, times

        times = 1
        if (present(arg)) times = arg

        do i = 1, times
            call next_single_property_change(point(), value_type(name="tabulated_list_column_name"), next, found, st)
            if (.not. found) next = point_max()
            if (next <= pos_eol()) call goto_char(next)
        end do
    end subroutine tabulated_list_next_column

    recursive subroutine cmd_tabulated_list_next_column(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call tabulated_list_next_column(int(prefix_numeric_value(current_prefix_arg)))
    end subroutine cmd_tabulated_list_next_column

    !> Go to the start of the column point is in on the current line.
    !> If ARG is provided, move that many columns.  Port of
    !> tabulated-list.el:tabulated-list-previous-column.
    subroutine tabulated_list_previous_column(arg)
        integer, intent(in), optional :: arg
        integer(int64) :: prev
        type(status_type) :: st
        logical :: found
        integer :: i, times

        times = 1
        if (present(arg)) times = arg

        do i = 1, times
            call previous_single_property_change(point(), value_type(name="tabulated_list_column_name"), prev, found, st)
            if (.not. found) prev = 1
            if (.not. (prev < pos_bol())) call goto_char(prev)
        end do
    end subroutine tabulated_list_previous_column

    recursive subroutine cmd_tabulated_list_previous_column(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call tabulated_list_previous_column(int(prefix_numeric_value(current_prefix_arg)))
    end subroutine cmd_tabulated_list_previous_column

    ! ------------------------------------------------------------------
    ! The mode
    ! ------------------------------------------------------------------

    !> Generic major mode for browsing a list of items.
    !> This mode is usually not used directly; instead, other major modes
    !> are derived from it.
    !>
    !> In this major mode, the buffer is divided into multiple columns,
    !> which are labeled using the header line.  Each non-empty line belongs
    !> to one "entry", and the entries can be sorted according to their
    !> column values.
    !>
    !> An inheriting mode should usually do the following in their body:
    !>
    !>  - Set `tabulated_list_format', specifying the column format.
    !>  - Set `tabulated_list_revert_hook', if the buffer contents need to
    !>    be specially recomputed prior to `revert_buffer'.
    !>  - Maybe set a `tabulated_list_entries' function (see below).
    !>  - Maybe set `tabulated_list_printer' (see below).
    !>  - Maybe set `tabulated_list_padding'.
    !>  - Call `tabulated_list_init_header' to initialize
    !>    `header_line_format' according to `tabulated_list_format'.
    !>
    !> An inheriting mode is usually accompanied by a "list-FOO" command
    !> (e.g. `list_packages', `list_processes').  This command creates or
    !> switches to a buffer and enables the major mode in that buffer.  If
    !> `tabulated_list_entries' is not a function, the command should
    !> initialize it to a list of entries for displaying.  Finally, it
    !> should call `tabulated_list_print'.
    !>
    !> `tabulated_list_print' calls the printer function specified by
    !> `tabulated_list_printer', once for each entry.  The default printer
    !> is `tabulated_list_print_entry'.
    !>
    !> Port of tabulated-list.el:tabulated-list-mode.
    recursive subroutine tabulated_list_mode(status)
        type(status_type), intent(out) :: status
        integer :: k
        logical :: saved

        call major_modes%tabulated_list_mode%begin(saved, status)
        if (.not. status%ok()) return
        ! (truncate-lines: not ported.)
        buffers(current)%undo_enabled = .false.
        call revert_buffer_function%set_local(revert_function)
        k = local_index(.true.)
        locals(k)%original_order = .false.
        call major_modes%tabulated_list_mode%end(saved, status)
    end subroutine tabulated_list_mode

    !> Define the mode, its keymap and options, and register the commands.
    !> Not a port.
    subroutine syms_of_tabulated_list()
        type(status_type) :: st

        if (.not. allocated(tabulated_list_gui_sort_indicator_asc)) then
            tabulated_list_gui_sort_indicator_asc = char(226)//char(150)//char(188)
        end if

        if (.not. allocated(tabulated_list_gui_sort_indicator_desc)) then
            tabulated_list_gui_sort_indicator_desc = char(226)//char(150)//char(178)
        end if

        if (.not. allocated(tabulated_list_tty_sort_indicator_asc)) tabulated_list_tty_sort_indicator_asc = "v"
        if (.not. allocated(tabulated_list_tty_sort_indicator_desc)) tabulated_list_tty_sort_indicator_desc = "^"
        call tabulated_list_fake_header%define("tabulated_list_fake_header", "Face used on fake header lines.")
        call tabulated_list_fake_header%spec_entry(display_when(), face_attrs(overline=.true., underline=.true., &
                                                                              weight="bold"))
        call append_locals_clearer(clear_tabulated_list_locals)
        call major_modes%tabulated_list_mode%define("tabulated_list_mode", "Tabulated", tabulated_list_mode, &
                                                    tabulated_list_mode_hook, parent=major_modes%special_mode)
        tabulated_list_mode_map = major_modes%tabulated_list_mode%map
        call keymap_set(tabulated_list_mode_map, "n", "next_line", st)
        call keymap_set(tabulated_list_mode_map, "p", "previous_line", st)
        call keymap_set(tabulated_list_mode_map, "M-<left>", "tabulated_list_previous_column", st)
        call keymap_set(tabulated_list_mode_map, "M-<right>", "tabulated_list_next_column", st)
        call keymap_set(tabulated_list_mode_map, "S", "tabulated_list_sort", st)
        call keymap_set(tabulated_list_mode_map, "}", "tabulated_list_widen_current_column", st)
        call keymap_set(tabulated_list_mode_map, "{", "tabulated_list_narrow_current_column", st)
        commands%tabulated_list_mode%execute => tabulated_list_mode
        commands%tabulated_list_revert%execute => tabulated_list_revert
        commands%tabulated_list_sort%execute => cmd_tabulated_list_sort
        commands%tabulated_list_widen_current_column%execute => cmd_tabulated_list_widen_current_column
        commands%tabulated_list_narrow_current_column%execute => cmd_tabulated_list_narrow_current_column
        commands%tabulated_list_next_column%execute => cmd_tabulated_list_next_column
        commands%tabulated_list_previous_column%execute => cmd_tabulated_list_previous_column
    end subroutine syms_of_tabulated_list
end module tabulated_list_fm
