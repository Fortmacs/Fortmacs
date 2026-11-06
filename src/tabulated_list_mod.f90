! tabulated_list_mod.f90 -- generic major mode for tabulated lists.
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
!> (`tabulated_list_format%set_local': an array of `tabulated_list_column_type'),
!> its entries (`tabulated_list_entries%set_local': an array of
!> `tabulated_list_entry_type', or a procedure that makes them), maybe its
!> padding and sort key, calls `tabulated_list_initialize_header' and then
!> `tabulated_list_print'.
!>
!> Each entry has an ID, a string identifying it (upstream: any Lisp
!> object, compared with `equal'), and one column descriptor (a
!> `tabulated_list_description_type': a string, maybe with a face) per column.
!> A column sorts by its descriptors' strings (SORT true) or by a
!> TEST on two entries.
!>
!> Deferred: the fake header for `tabulated_list_use_header_line' false,
!> image and button column descriptors and `button_buffer_map' in the
!> mode's keymap, the sort buttons of the header line (mouse clicks, and
!> `tabulated_list_column_sort'), groups (`tabulated_list_groups' and its
!> functions), `truncate_lines' (not ported: long lines wrap), the
!> help-echo and mouse-face of columns, `header_line_indent_mode' (there
!> are no line numbers), bidi.
module tabulated_list_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use execute_interactively_mod, only: PREFIX_NONE
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, USER_ERROR_CODE
    use handle_mod, only: handle_type
    use value_mod, only: value_type, make_list, xstring, xfixnum, string_intervals, vtag, V_INTEGER, V_STRING
    use utility_functions_mod, only: is_string_less
    use data_mod, only: append_locals_clearer, forward_setting, integer_setting_type, logical_setting_type
    use hooks_mod, only: hook_type
    use modes_mod, only: major_modes
    use commands_mod, only: commands
    use buffer_mod, only: buffers, current, erase_buffer, restore_buffer_modified
    use buffer_local_setting_mod, only: buffer_local_setting_type
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, position_beginning_of_line, &
                                     position_end_of_line, delete_region, is_end_of_buffer
    use editing_commands_mod, only: forward_line, beginning_of_line, forward_character
    use indent_mod, only: current_column, move_to_column
    use text_properties_mod, only: get_text_property, put_text_property, add_text_properties, next_single_property_change, &
                                   previous_single_property_change, propertize
    use files_mod, only: revert_buffer_function
    use fill_mod, only: string_width
    use frame_mod, only: selected_frame
    use derived_mod, only: is_derived_mode
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use multilingual_environment_utilities_mod, only: truncate_string_to_width, truncate_string_to_width_propertized
    use faces_mod, only: named_face_type, face_attributes, display_when
    use keymap_mod, only: keymap_set
    implicit none(type, external)
    private

    public :: tabulated_list_mode, tabulated_list_entries_procedure, tabulated_list_printer_procedure, &
              tabulated_list_get_id, tabulated_list_get_entry, tabulated_list_put_tag, tabulated_list_clear_all_tags, &
              tabulated_list_initialize_header, tabulated_list_revert, tabulated_list_print, tabulated_list_print_entry, &
              tabulated_list_print_column, tabulated_list_delete_entry, tabulated_list_set_column, tabulated_list_sort, &
              tabulated_list_widen_current_column, tabulated_list_narrow_current_column, tabulated_list_next_column, &
              tabulated_list_previous_column, initialize_tabulated_list

    !> A column of the format: its NAME (the label in the header line;
    !> different columns must have different names) and the WIDTH to
    !> reserve for it (for the final column, its value is ignored).  SORT
    !> true means it can sort the entries: by its descriptors' strings, or
    !> by TEST if that is associated.  RIGHT_ALIGN true means the
    !> column is right-aligned; PAD_RIGHT is the number of additional
    !> padding spaces to its right.  Port of an element (NAME WIDTH SORT .
    !> PROPERTIES) of tabulated-list.el tabulated-list-format.
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
    type, public :: tabulated_list_description_type
        character(len=:), allocatable :: text
        character(len=:), allocatable :: face
    end type tabulated_list_description_type

    !> An entry: its ID, which keeps the cursor on the "same" entry when
    !> rearranging the list (empty for none), and one descriptor per column.
    !> Port of an element (ID [DESC1 ... DESCN]) of tabulated-list.el
    !> tabulated-list-entries.
    type, public :: tabulated_list_entry_type
        character(len=:), allocatable :: id
        type(tabulated_list_description_type), allocatable :: columns(:)
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

    !> Indicator for columns sorted in ascending order, for GRAPHICAL_USER_INTERFACE frames.
    !> See `tabulated_list_terminal_sort_indicator_asc' for the indicator used
    !> on text-mode frames.  Port of tabulated-list.el
    !> tabulated-list-gui-sort-indicator-asc.
    character(len=:), allocatable, public :: tabulated_list_graphical_sort_indicator_asc

    !> Indicator for columns sorted in descending order, for GRAPHICAL_USER_INTERFACE frames.
    !> See `tabulated_list_terminal_sort_indicator_description' for the indicator used
    !> on text-mode frames.  Port of tabulated-list.el
    !> tabulated-list-gui-sort-indicator-desc.
    character(len=:), allocatable, public :: tabulated_list_graphical_sort_indicator_description

    !> Indicator for columns sorted in ascending order, for text-mode
    !> frames.  See `tabulated_list_graphical_sort_indicator_asc' for the
    !> indicator used on GRAPHICAL_USER_INTERFACE frames.  Port of tabulated-list.el
    !> tabulated-list-tty-sort-indicator-asc.
    character(len=:), allocatable, public :: tabulated_list_terminal_sort_indicator_asc

    !> Indicator for columns sorted in ascending order, for text-mode
    !> frames.  See `tabulated_list_graphical_sort_indicator_asc' for the
    !> indicator used on GRAPHICAL_USER_INTERFACE frames.  Port of tabulated-list.el
    !> tabulated-list-tty-sort-indicator-desc.
    character(len=:), allocatable, public :: tabulated_list_terminal_sort_indicator_description

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

    !> One buffer's own `tabulated_list_format'.
    type :: tabulated_list_format_entry_type
        type(tabulated_list_column_type), allocatable :: columns(:)
    end type tabulated_list_format_entry_type

    !> The type of `tabulated_list_format': the global value `columns'
    !> (empty unless set) and the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: tabulated_list_format_setting_type
        type(tabulated_list_column_type), allocatable :: columns(:)
        type(tabulated_list_format_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value, COLUMNS.
        procedure :: set_local => set_tabulated_list_format_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_tabulated_list_format_current
        procedure :: append_local_entry => append_tabulated_list_format_entry
        procedure :: clear_local_entry => clear_tabulated_list_format_entry
    end type tabulated_list_format_setting_type

    !> One buffer's own `tabulated_list_entries'.
    type :: tabulated_list_entries_entry_type
        !> The entries; while `procedure' is associated, the entries it
        !> made when the buffer was last printed.
        type(tabulated_list_entry_type), allocatable :: entries(:)
        procedure(tabulated_list_entries_procedure), pointer, nopass :: procedure => null()
        !> Whether tabulated-list--original-order is set: the entries'
        !> `original_index' gives the order they were first displayed in.
        logical :: original_order = .false.
    end type tabulated_list_entries_entry_type

    !> The type of `tabulated_list_entries': the global value (`entries',
    !> or `procedure' making them; empty unless set) and the buffers' own
    !> values (`set_local', with an array of entries or a procedure).
    type, extends(buffer_local_setting_type), public :: tabulated_list_entries_setting_type
        type(tabulated_list_entry_type), allocatable :: entries(:)
        procedure(tabulated_list_entries_procedure), pointer, nopass :: procedure => null()
        type(tabulated_list_entries_entry_type), allocatable :: locals(:)
    contains
        procedure, private :: set_tabulated_list_entries_local
        procedure, private :: set_tabulated_list_entries_procedure_local
        !> Give the current buffer its own value: an array of entries, or
        !> a procedure that makes them.
        generic :: set_local => set_tabulated_list_entries_local, set_tabulated_list_entries_procedure_local
        !> The current buffer's entries as last printed (or set).
        procedure :: get_current => get_tabulated_list_entries_current
        procedure :: append_local_entry => append_tabulated_list_entries_entry
        procedure :: clear_local_entry => clear_tabulated_list_entries_entry
    end type tabulated_list_entries_setting_type

    !> One buffer's own `tabulated_list_sort_key'.
    type :: tabulated_list_sort_key_entry_type
        character(len=:), allocatable :: name
        logical :: flip = .false.
    end type tabulated_list_sort_key_entry_type

    !> The type of `tabulated_list_sort_key': the global value (`name',
    !> unallocated for no sort key, and `flip') and the buffers' own
    !> values (`set_local').
    type, extends(buffer_local_setting_type), public :: tabulated_list_sort_key_setting_type
        character(len=:), allocatable :: name
        logical :: flip = .false.
        type(tabulated_list_sort_key_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value, NAME and FLIP (no sort
        !> key when NAME is absent).
        procedure :: set_local => set_tabulated_list_sort_key_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_tabulated_list_sort_key_current
        procedure :: append_local_entry => append_tabulated_list_sort_key_entry
        procedure :: clear_local_entry => clear_tabulated_list_sort_key_entry
    end type tabulated_list_sort_key_setting_type

    !> One buffer's own `tabulated_list_printer'.
    type :: tabulated_list_printer_entry_type
        procedure(tabulated_list_printer_procedure), pointer, nopass :: procedure => null()
    end type tabulated_list_printer_entry_type

    !> The type of `tabulated_list_printer': the global value `procedure'
    !> (set with `=>') and the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: tabulated_list_printer_setting_type
        procedure(tabulated_list_printer_procedure), pointer, nopass :: procedure => null()
        type(tabulated_list_printer_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_tabulated_list_printer_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_tabulated_list_printer_current
        procedure :: append_local_entry => append_tabulated_list_printer_entry
        procedure :: clear_local_entry => clear_tabulated_list_printer_entry
    end type tabulated_list_printer_setting_type

    !> The format of the current Tabulated List mode buffer.
    !> This should be an array of `tabulated_list_column_type': the NAME
    !> of each column (the label in the header line; different columns
    !> must have different names), the WIDTH to reserve for it, and
    !> whether it can SORT the entries.  Port of tabulated-list.el
    !> tabulated-list-format (always buffer-local and permanent: a mode
    !> gives its buffer its own with `%set_local').
    type(tabulated_list_format_setting_type), public :: tabulated_list_format

    !> Entries displayed in the current Tabulated List buffer.
    !> This should be either a procedure (`%set_local' with a procedure),
    !> or an array of `tabulated_list_entry_type' (`%set_local' with the
    !> array): an ID, which keeps the cursor on the "same" entry when
    !> rearranging the list, and one column descriptor per column of
    !> `tabulated_list_format'.  If it is a procedure, it is called with
    !> no arguments and makes the entries.  Port of tabulated-list.el
    !> tabulated-list-entries (always buffer-local and permanent).
    type(tabulated_list_entries_setting_type), public :: tabulated_list_entries

    !> Number of characters preceding each Tabulated List mode entry.
    !> By default, lines are padded with spaces, but you can use the
    !> function `tabulated_list_put_tag' to change this.  Port of
    !> tabulated-list.el tabulated-list-padding (local when set and
    !> permanently local: `%set_local(width)').
    type(integer_setting_type), target, public :: tabulated_list_padding = &
                                                  integer_setting_type(name="tabulated_list_padding", permanent_local=.true.)

    !> Sort key for the current Tabulated List mode buffer.
    !> If no sort key (`name' unallocated), no additional sorting is
    !> performed.  Otherwise, NAME is one of the column names in
    !> `tabulated_list_format' (the corresponding SORT entry then
    !> specifies how to sort), and FLIP true means to invert the
    !> resulting sort.  Port of tabulated-list.el tabulated-list-sort-key
    !> (always buffer-local and permanent; `%set_local(name, flip)').
    type(tabulated_list_sort_key_setting_type), public :: tabulated_list_sort_key

    !> Whether the Tabulated List buffer should use a header line.
    !> Port of tabulated-list.el tabulated-list-use-header-line (local
    !> when set: `%set_local'; .false., the fake header, is Deferred).
    type(logical_setting_type), target, public :: tabulated_list_use_header_line = &
                                                  logical_setting_type(name="tabulated_list_use_header_line", value=.true.)

    !> Function for inserting a Tabulated List entry at point.
    !> It is called with one argument, the entry (ID and column
    !> descriptors), and should insert a line describing it; the default
    !> is `tabulated_list_print_entry'.  Port of tabulated-list.el
    !> tabulated-list-printer (always buffer-local; `%set_local').
    type(tabulated_list_printer_setting_type), public :: tabulated_list_printer

    !> tabulated-list--near-rows: the descriptors of the previous, current
    !> and next rows while printing (for the available space of a column
    !> before a right-aligned one).
    type(tabulated_list_entry_type) :: near_rows(3)
    logical :: has_near_rows = .false.

contains

    ! ------------------------------------------------------------------
    ! Buffer-local variables
    ! ------------------------------------------------------------------

    ! kill-all-local-variables: forget the current buffer's values but
    ! the permanent-local ones.  Not a port.
    subroutine clear_tabulated_list_locals()
        integer :: k

        call tabulated_list_printer%clear_local()
        k = tabulated_list_entries%get_local_index()
        if (k > 0) tabulated_list_entries%locals(k)%original_order = .false.
    end subroutine clear_tabulated_list_locals

    ! Not a port.
    subroutine copy_columns(from, to)
        type(tabulated_list_column_type), intent(in) :: from(:)
        type(tabulated_list_column_type), allocatable, intent(out) :: to(:)
        integer :: j

        allocate (to(size(from)))

        do j = 1, size(from)
            to(j) = from(j)
        end do
    end subroutine copy_columns

    !> Give the current buffer its own `tabulated_list_format', COLUMNS.
    !> Not a port.
    subroutine set_tabulated_list_format_local(self, columns)
        class(tabulated_list_format_setting_type), intent(inout) :: self
        type(tabulated_list_column_type), intent(in) :: columns(:)
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        call copy_columns(columns, self%locals(k)%columns)
    end subroutine set_tabulated_list_format_local

    !> The current buffer's `tabulated_list_format', COLUMNS: its own value, else
    !> the global one (empty when unset).  Not a port.
    subroutine get_tabulated_list_format_current(self, columns)
        class(tabulated_list_format_setting_type), intent(in) :: self
        type(tabulated_list_column_type), allocatable, intent(out) :: columns(:)
        integer :: k

        k = self%get_local_index()

        if (k > 0) then
            if (allocated(self%locals(k)%columns)) then
                call copy_columns(self%locals(k)%columns, columns)
                return
            end if
        else if (allocated(self%columns)) then
            call copy_columns(self%columns, columns)
            return
        end if

        allocate (columns(0))
    end subroutine get_tabulated_list_format_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_tabulated_list_format_entry(self)
        class(tabulated_list_format_setting_type), intent(inout) :: self
        type(tabulated_list_format_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_tabulated_list_format_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_tabulated_list_format_entry(self, k)
        class(tabulated_list_format_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        if (allocated(self%locals(k)%columns)) deallocate (self%locals(k)%columns)
    end subroutine clear_tabulated_list_format_entry

    !> Give the current buffer its own `tabulated_list_entries', the
    !> array ENTRIES.  Not a port.
    subroutine set_tabulated_list_entries_local(self, entries)
        class(tabulated_list_entries_setting_type), intent(inout) :: self
        type(tabulated_list_entry_type), intent(in) :: entries(:)
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        call copy_entries(entries, self%locals(k)%entries)
        self%locals(k)%procedure => null()
    end subroutine set_tabulated_list_entries_local

    !> Give the current buffer its own `tabulated_list_entries', the
    !> procedure LOCAL_PROCEDURE that makes them.  Not a port.
    subroutine set_tabulated_list_entries_procedure_local(self, local_procedure)
        class(tabulated_list_entries_setting_type), intent(inout) :: self
        procedure(tabulated_list_entries_procedure) :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => local_procedure
    end subroutine set_tabulated_list_entries_procedure_local

    !> The current buffer's `tabulated_list_entries', ENTRIES, as last printed (or
    !> set): its own, else the global array.  Not a port.
    subroutine get_tabulated_list_entries_current(self, entries)
        class(tabulated_list_entries_setting_type), intent(in) :: self
        type(tabulated_list_entry_type), allocatable, intent(out) :: entries(:)
        integer :: k

        k = self%get_local_index()

        if (k > 0) then
            if (allocated(self%locals(k)%entries)) then
                call copy_entries(self%locals(k)%entries, entries)
                return
            end if
        else if (allocated(self%entries)) then
            call copy_entries(self%entries, entries)
            return
        end if

        allocate (entries(0))
    end subroutine get_tabulated_list_entries_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_tabulated_list_entries_entry(self)
        class(tabulated_list_entries_setting_type), intent(inout) :: self
        type(tabulated_list_entries_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_tabulated_list_entries_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_tabulated_list_entries_entry(self, k)
        class(tabulated_list_entries_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        if (allocated(self%locals(k)%entries)) deallocate (self%locals(k)%entries)
        self%locals(k)%procedure => null()
        self%locals(k)%original_order = .false.
    end subroutine clear_tabulated_list_entries_entry

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

    ! The index K of the current buffer's own entry of
    ! `tabulated_list_entries', made if it has none (0 without a current
    ! buffer), with its entries allocated.  Not a port.
    subroutine make_entries_local(k)
        integer, intent(out) :: k

        call tabulated_list_entries%make_local_entry(k)
        if (k == 0) return
        if (.not. allocated(tabulated_list_entries%locals(k)%entries)) allocate (tabulated_list_entries%locals(k)%entries(0))
    end subroutine make_entries_local

    !> Give the current buffer its own `tabulated_list_sort_key': NAME,
    !> one of the column names in `tabulated_list_format', and FLIP,
    !> true to invert the resulting sort; without NAME, no sort key.
    !> Not a port.
    subroutine set_tabulated_list_sort_key_local(self, name, flip)
        class(tabulated_list_sort_key_setting_type), intent(inout) :: self
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: flip
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        call self%clear_local_entry(k)
        if (present(name)) self%locals(k)%name = name
        if (present(flip)) self%locals(k)%flip = flip
    end subroutine set_tabulated_list_sort_key_local

    !> The current buffer's `tabulated_list_sort_key': its NAME (empty
    !> for none) and FLIP.  Not a port.
    subroutine get_tabulated_list_sort_key_current(self, name, flip)
        class(tabulated_list_sort_key_setting_type), intent(in) :: self
        character(len=:), allocatable, intent(out) :: name
        logical, intent(out) :: flip
        integer :: k

        name = ""
        flip = self%flip
        if (allocated(self%name)) name = self%name
        k = self%get_local_index()
        if (k == 0) return
        name = ""
        if (allocated(self%locals(k)%name)) name = self%locals(k)%name
        flip = self%locals(k)%flip
    end subroutine get_tabulated_list_sort_key_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_tabulated_list_sort_key_entry(self)
        class(tabulated_list_sort_key_setting_type), intent(inout) :: self
        type(tabulated_list_sort_key_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_tabulated_list_sort_key_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_tabulated_list_sort_key_entry(self, k)
        class(tabulated_list_sort_key_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        if (allocated(self%locals(k)%name)) deallocate (self%locals(k)%name)
        self%locals(k)%flip = .false.
    end subroutine clear_tabulated_list_sort_key_entry

    !> Give the current buffer its own `tabulated_list_printer',
    !> LOCAL_PROCEDURE (null when absent).  Not a port.
    subroutine set_tabulated_list_printer_local(self, local_procedure)
        class(tabulated_list_printer_setting_type), intent(inout) :: self
        procedure(tabulated_list_printer_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_tabulated_list_printer_local

    !> The current buffer's `tabulated_list_printer': its own value, else
    !> the global one.  Not a port.
    function get_tabulated_list_printer_current(self) result(current_procedure)
        class(tabulated_list_printer_setting_type), intent(in) :: self
        procedure(tabulated_list_printer_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_tabulated_list_printer_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_tabulated_list_printer_entry(self)
        class(tabulated_list_printer_setting_type), intent(inout) :: self
        type(tabulated_list_printer_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_tabulated_list_printer_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_tabulated_list_printer_entry(self, k)
        class(tabulated_list_printer_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_tabulated_list_printer_entry

    ! ------------------------------------------------------------------
    ! Entries at positions
    ! ------------------------------------------------------------------

    !> Return the entry ID of the Tabulated List entry at POSITION ("" for
    !> none).  POSITION, if absent, defaults to point.  Port of
    !> tabulated-list.el:tabulated-list-get-id.
    function tabulated_list_get_id(position) result(id)
        integer(int64), intent(in), optional :: position
        character(len=:), allocatable :: id
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: p

        id = ""
        p = point()
        if (present(position)) p = position
        if (p < point_minimum() .or. p >= point_maximum()) return
        call get_text_property(p, value_type(name="tabulated_list_id"), v, st)
        if (st%ok() .and. vtag(v) == V_STRING) id = xstring(v)
    end function tabulated_list_get_id

    !> Return in ENTRY the Tabulated List entry at POSITION; FOUND false if
    !> there is no entry at POSITION.  POSITION, if absent, defaults to point.
    !> Port of tabulated-list.el:tabulated-list-get-entry.
    subroutine tabulated_list_get_entry(entry, found, position)
        type(tabulated_list_entry_type), intent(out) :: entry
        logical, intent(out) :: found
        integer(int64), intent(in), optional :: position
        integer :: k, i

        k = entry_index(position)
        found = k > 0
        if (.not. found) return
        i = tabulated_list_entries%get_local_index()
        entry = tabulated_list_entries%locals(i)%entries(k)
    end subroutine tabulated_list_get_entry

    ! The index in the current buffer's entries of the entry at POSITION
    ! (point if absent), 0 for none.  Not a port.
    integer function entry_index(position) result(k)
        integer(int64), intent(in), optional :: position
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: p
        integer :: i

        k = 0
        p = point()
        if (present(position)) p = position
        if (p < point_minimum() .or. p >= point_maximum()) return
        call get_text_property(p, value_type(name="tabulated_list_entry"), v, st)
        if (.not. st%ok() .or. vtag(v) /= V_INTEGER) return
        i = tabulated_list_entries%get_local_index()
        if (i == 0) return
        if (.not. allocated(tabulated_list_entries%locals(i)%entries)) return
        k = int(xfixnum(v))
        if (k < 1 .or. k > size(tabulated_list_entries%locals(i)%entries)) k = 0
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
        padding = int(tabulated_list_padding%value)

        if (.not. (padding > 0)) then
            call set_error(status, GENERIC_ERROR_CODE, "Unable to tag the current line")
            return
        end if

        saved = point()
        call beginning_of_line()

        if (entry_index() > 0) then
            beginning = point()
            ro = buffers(current)%read_only
            buffers(current)%read_only = .false.
            call goto_character(beginning + padding)
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

        call goto_character(saved)

        if (present(advance)) then
            if (advance) shortage = forward_line()
        end if
    end subroutine tabulated_list_put_tag

    ! Insert TEXT at point with the text properties of the character
    ! after it (as insert-and-inherit takes those of the line's entry).
    ! Not a port.
    recursive subroutine insert_inheriting(text, status)
        use text_properties_mod, only: text_properties_at
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
        use search_mod, only: regular_expression_search_forward
        type(status_type), intent(out) :: status
        integer(int64) :: saved, found
        integer :: padding
        character(len=24) :: buffer
        logical :: ro

        status = status_type()
        padding = int(tabulated_list_padding%value)

        if (.not. (padding > 0)) then
            call set_error(status, GENERIC_ERROR_CODE, "There can be no tags in current buffer")
            return
        end if

        saved = point()
        call goto_character(point_minimum())
        ro = buffers(current)%read_only
        buffers(current)%read_only = .false.
        ! Match non-space in the first n characters.
        write (buffer, "(i0)") padding - 1

        do
            call regular_expression_search_forward("^ \{0,"//trim(buffer)//"\}[^ ]", found, status, noerror=1)
            if (.not. status%ok() .or. found == 0) exit
            call tabulated_list_put_tag(repeat(" ", padding), status)
            if (.not. status%ok()) exit
        end do

        buffers(current)%read_only = ro
        call goto_character(saved)
    end subroutine tabulated_list_clear_all_tags

    ! ------------------------------------------------------------------
    ! The header
    ! ------------------------------------------------------------------

    !> Set up header line for the Tabulated List buffer.  Port of
    !> tabulated-list.el:tabulated-list-init-header (the header line's
    !> segments; the sort indicator of a text-mode frame is the one its
    !> glyphless-char-display table shows upstream).
    subroutine tabulated_list_initialize_header()
        use buffer_mod, only: header_line_format_type
        type(header_line_format_type) :: header
        type(tabulated_list_column_type), allocatable :: format(:)
        character(len=:), allocatable :: label, pname, sort_name, indicator
        integer :: x, n, length_fmt, lablen, next_x, available_space, shift, width
        logical :: not_last_column, flip

        call tabulated_list_format%get_current(format)
        call tabulated_list_sort_key%get_current(sort_name, flip)
        x = max(int(tabulated_list_padding%value), 0)
        length_fmt = size(format)
        ! Pre-compute width for available-space computation.
        has_near_rows = .true.
        call header_row(format, near_rows(1))
        call header_row(format, near_rows(2))
        call header_row(format, near_rows(3))
        call header%append("", align_to=x)

        do n = 1, length_fmt
            not_last_column = n < length_fmt
            label = format(n)%name
            lablen = int(string_width(label))
            pname = label
            width = format(n)%width
            next_x = x + format(n)%pad_right + width
            available_space = 0

            if (not_last_column) then
                if (format(n)%right_align) then
                    available_space = width
                else
                    available_space = tabulated_list_available_space(format, width, n)
                end if
            end if

            if (lablen >= 3 .and. not_last_column .and. lablen > available_space) then
                label = truncate_string_to_width(label, available_space, default_ellipsis=.true.)
            end if

            if (.not. format(n)%sort) then
                ! An unsortable column
                call append(label, "")
            else if (same(pname, sort_name)) then
                ! The selected sort column
                indicator = ""

                if (.not. (lablen < 3 .and. not_last_column)) then
                    if (flip) then
                        indicator = " "//description_indicator()
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

        if (tabulated_list_use_header_line%value) then
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
    end subroutine tabulated_list_initialize_header

    ! The column names as a row of descriptors (hcols).  Not a port.
    subroutine header_row(format, row)
        type(tabulated_list_column_type), intent(in) :: format(:)
        type(tabulated_list_entry_type), intent(out) :: row
        integer :: j

        allocate (row%columns(size(format)))

        do j = 1, size(format)
            row%columns(j)%text = format(j)%name
            row%columns(j)%face = ""
        end do
    end subroutine header_row

    ! Not a port.
    function asc_indicator() result(s)
        character(len=:), allocatable :: s

        if (associated(selected_frame%redisplay_interface)) then
            s = tabulated_list_graphical_sort_indicator_asc
        else
            s = tabulated_list_terminal_sort_indicator_asc
        end if
    end function asc_indicator

    ! Not a port.
    function description_indicator() result(s)
        character(len=:), allocatable :: s

        if (associated(selected_frame%redisplay_interface)) then
            s = tabulated_list_graphical_sort_indicator_description
        else
            s = tabulated_list_terminal_sort_indicator_description
        end if
    end function description_indicator

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
            call set_error(status, GENERIC_ERROR_CODE, "The current buffer is not in Tabulated List mode")
            return
        end if

        call tabulated_list_revert_hook%execute(status)
        if (.not. status%ok()) return
        call tabulated_list_print(status, remember_position=.true.)
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
        call set_error(status, GENERIC_ERROR_CODE, "No column named "//name)
    end function column_number

    ! Whether entry A sorts before entry B by column N of FORMAT, the
    ! other way round when FLIP is true.
    ! Port of tabulated-list.el:tabulated-list--get-sorter.
    logical function sorts_before(format, flip, n, a, b) result(before)
        type(tabulated_list_column_type), intent(in) :: format(:)
        logical, intent(in) :: flip
        integer, intent(in) :: n
        type(tabulated_list_entry_type), intent(in) :: a, b

        if (flip) then
            before = sorter(b, a)
        else
            before = sorter(a, b)
        end if

    contains

        logical function sorter(x, y)
            type(tabulated_list_entry_type), intent(in) :: x, y

            if (associated(format(n)%test)) then
                sorter = format(n)%test(x, y)
            else
                ! Default sorter checks column N:
                sorter = is_string_less(x%columns(n)%text, y%columns(n)%text)
            end if
        end function sorter
    end function sorts_before

    ! The sort column in FORMAT of the current buffer's sort key, and
    ! whether to FLIP the sort (N 0 for no sorter: no sort key, or an
    ! unsortable column).
    ! Port of part of tabulated-list.el:tabulated-list--get-sorter.
    integer function sort_column(format, flip, status) result(n)
        type(tabulated_list_column_type), intent(in) :: format(:)
        logical, intent(out) :: flip
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        n = 0
        status = status_type()
        call tabulated_list_sort_key%get_current(name, flip)
        if (len(name) == 0) return
        n = column_number(format, name, status)

        if (n > 0) then
            if (.not. format(n)%sort) n = 0
        end if
    end function sort_column

    ! Sort ENTRIES stably by column N of FORMAT (the other way round when
    ! FLIP is true).  Not a port.
    subroutine sort_entries(format, flip, n, entries)
        type(tabulated_list_column_type), intent(in) :: format(:)
        logical, intent(in) :: flip
        integer, intent(in) :: n
        type(tabulated_list_entry_type), intent(inout) :: entries(:)
        type(tabulated_list_entry_type) :: t
        integer :: i, j

        do i = 2, size(entries)
            t = entries(i)
            j = i - 1

            do while (j >= 1)
                if (.not. sorts_before(format, flip, n, t, entries(j))) exit
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
    !> Optional argument REMEMBER_POSITION, if true, means to move point to the
    !> entry with the same ID element as the current line.
    !>
    !> True UPDATE argument means to use an alternative printing method
    !> which is faster if most entries haven't changed since the last
    !> print.  The only difference in outcome is that tags will not be
    !> removed from entries that haven't changed (see
    !> `tabulated_list_put_tag').  Don't use this immediately after changing
    !> `tabulated_list_sort_key'.  Port of
    !> tabulated-list.el:tabulated-list-print (no groups).
    recursive subroutine tabulated_list_print(status, remember_position, update)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: remember_position, update
        type(tabulated_list_entry_type), allocatable :: entries(:)
        type(tabulated_list_column_type), allocatable :: format(:)
        procedure(tabulated_list_entries_procedure), pointer :: make_entries
        procedure(tabulated_list_printer_procedure), pointer :: printer
        character(len=:), allocatable :: entry_id
        integer(int64) :: saved_point, saved_column, reached, shortage
        integer :: k, n, i
        logical :: remember, upd, ro, has_id, flip

        call make_entries_local(k)
        remember = .false.
        if (present(remember_position)) remember = remember_position
        upd = .false.
        if (present(update)) upd = update
        make_entries => tabulated_list_entries%locals(k)%procedure

        if (associated(make_entries)) then
            call make_entries(entries, status)
            if (.not. status%ok()) return
            ! (The procedure may have given other buffers their own entries.)
            call make_entries_local(k)
        else
            call copy_entries(tabulated_list_entries%locals(k)%entries, entries)
        end if

        call tabulated_list_format%get_current(format)
        n = sort_column(format, flip, status)
        if (.not. status%ok()) return
        has_id = .false.
        saved_column = 0

        if (remember) then
            entry_id = tabulated_list_get_id()
            has_id = len(entry_id) > 0
            if (has_id) saved_column = current_column()
        end if

        ! Sort the entries, if necessary.
        if (n > 0) call sort_entries(format, flip, n, entries)
        ! (The entries as printed, also when a procedure made them.)
        call copy_entries(entries, tabulated_list_entries%locals(k)%entries)
        printer => tabulated_list_printer%get_current()
        ! Without a sorter, we have no way to just update.
        if (upd .and. n == 0) upd = .false.
        ro = buffers(current)%read_only
        buffers(current)%read_only = .false.

        if (upd) then
            call goto_character(point_minimum())
        else
            ! Redo the buffer, unless we're just updating.
            call erase_buffer(status)
        end if

        ! Finally, print the resulting list.
        saved_point = 0

        do i = 1, size(entries)
            call print_one(i)
            if (.not. status%ok()) exit
        end do

        if (upd .and. status%ok()) call delete_region(point(), point_maximum(), status)
        buffers(current)%read_only = ro
        if (.not. status%ok()) return
        call restore_buffer_modified(.false.)

        ! If REMEMBER_POSITION was specified, move to the "old" location.
        if (saved_point > 0) then
            call goto_character(saved_point)
            call move_to_column(saved_column, reached)
        else
            call goto_character(point_minimum())
        end if

    contains

        ! Port of the loop of tabulated-list.el:tabulated-list-print-entries.
        recursive subroutine print_one(i)
            integer, intent(in) :: i
            integer :: here
            character(len=:), allocatable :: local_id

            has_near_rows = .true.
            here = entry_index(position_beginning_of_line(0_int64))

            if (here > 0 .and. position_beginning_of_line(0_int64) < point()) then
                near_rows(1) = tabulated_list_entries%locals(k)%entries(here)
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
                    saved_point = point()
                end if
            end if

            ! If the buffer is empty, simply print each elt.
            if (.not. upd .or. is_end_of_buffer()) then
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

                    if (sorts_before(format, flip, n, entries(i), tabulated_list_entries%locals(k)%entries(entry_index()))) then
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

            if (associated(printer)) then
                call printer(entries(i), status)
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
        type(value_type) :: properties(2)
        integer(int64) :: beginning
        integer :: x, n, padding
        logical :: ro, saved_near, changed

        beginning = point()
        call tabulated_list_format%get_current(format)
        padding = int(tabulated_list_padding%value)
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
            if (n > size(entry%columns)) exit
            x = tabulated_list_print_column(n, entry%columns(n), x, status)
            if (.not. status%ok()) exit
        end do

        has_near_rows = saved_near
        if (status%ok()) call insert(achar(10), status)

        if (status%ok()) then
            properties(1) = value_type(name="tabulated_list_id")
            properties(2) = value_type(text=entry%id)
            call add_text_properties(beginning, point(), make_list(properties), changed, status)
        end if

        buffers(current)%read_only = ro
    end subroutine tabulated_list_print_entry

    ! The space available for column N of WIDTH: more than WIDTH when the
    ! next column is right-aligned and has room left.  Port of
    ! tabulated-list.el:tabulated-list--available-space.
    integer function tabulated_list_available_space(format, width, n) result(space)
        type(tabulated_list_column_type), intent(in) :: format(:)
        integer, intent(in) :: width, n
        integer :: next_column_width

        if (format(n + 1)%right_align) then
            next_column_width = format(n + 1)%width
            space = width + next_column_width - min(next_column_width, column_local_maximum_widths(n + 1))
        else
            space = width
        end if
    end function tabulated_list_available_space

    ! Return maximum entry widths at column COLUMN around current row.  Check
    ! the current row, the previous one and the next row.  Port of
    ! tabulated-list.el:tabulated-list--col-local-max-widths.
    integer function column_local_maximum_widths(column) result(w)
        integer, intent(in) :: column
        integer :: r

        w = 0

        do r = 1, 3
            if (.not. allocated(near_rows(r)%columns)) cycle
            if (column > size(near_rows(r)%columns)) cycle
            w = max(w, int(string_width(near_rows(r)%columns(column)%text)))
        end do
    end function column_local_maximum_widths

    !> Insert a specified Tabulated List entry at point.
    !> N is the column number, COLUMN_DESCRIPTION is a column descriptor (see
    !> `tabulated_list_entry_type'), and X is the column number at point.
    !> Return the column number after insertion.  Port of
    !> tabulated-list.el:tabulated-list-print-col (no images or buttons).
    recursive integer function tabulated_list_print_column(n, column_description, x_in, status) result(next_x)
        integer, intent(in) :: n, x_in
        type(tabulated_list_description_type), intent(in) :: column_description
        type(status_type), intent(out) :: status
        type(tabulated_list_column_type), allocatable :: format(:)
        type(value_type) :: label, properties(2), specification(3), align(2)
        integer(int64) :: original_point
        integer :: width, pad_right, label_width, available_space, shift, x
        logical :: not_last_column, right_align

        status = status_type()
        call tabulated_list_format%get_current(format)
        x = x_in
        width = format(n)%width
        pad_right = format(n)%pad_right
        right_align = format(n)%right_align
        label = value_type(text=column_description%text)
        label_width = int(string_width(column_description%text))
        original_point = point()
        not_last_column = n < size(format)
        available_space = 0

        if (not_last_column) then
            if (right_align) then
                available_space = width
            else
                available_space = tabulated_list_available_space(format, width, n)
            end if
        end if

        ! Truncate labels if necessary (except last column).  Don't
        ! truncate to `width' if the next column is align-right and has
        ! some space left, truncate to `available_space' instead.
        if (not_last_column .and. label_width > available_space) then
            label = truncate_string_to_width_propertized(column_description%text, available_space, default_ellipsis=.true.)
            label_width = available_space
        end if

        if (right_align .and. width > label_width) then
            shift = width - label_width
            call insert_spaces(shift, x + shift)
            if (.not. status%ok()) return
            width = width - shift
            x = x + shift
        end if

        if (len(column_description%face) > 0) then
            properties(1) = value_type(name="face")
            properties(2) = value_type(name=column_description%face)
            label = propertize(label, make_list(properties))
        end if

        call insert_value(label, status)
        if (.not. status%ok()) return
        next_x = x + pad_right + width

        ! No need to append any spaces if this is the last column.
        if (not_last_column) then
            if (pad_right > 0) call insert(repeat(" ", pad_right), status)
            if (.not. status%ok()) return
            call insert_spaces(width - min(width, label_width), next_x)
            if (.not. status%ok()) return
        end if

        call put_text_property(original_point, point(), value_type(name="tabulated_list_column_name"), &
                               value_type(text=format(n)%name), &
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
            specification(1) = value_type(name="space")
            specification(2) = value_type(name=":align-to")
            specification(3) = value_type(int(column, int64))
            align(1) = value_type(name="display")
            align(2) = make_list(specification)
            call put_text_property(beginning, point(), align(1), align(2), status)
        end subroutine insert_spaces
    end function tabulated_list_print_column

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
        call delete_region(position_beginning_of_line(), position_end_of_line() + 1, status)
        buffers(current)%read_only = ro
    end subroutine tabulated_list_delete_entry

    !> Change the Tabulated List entry at point, setting COLUMN to DESCRIPTION.
    !> COLUMN is the column number to change (1-based), or the name of the
    !> column to change (COLUMN_NAME).  DESCRIPTION is the new column descriptor,
    !> which is inserted via `tabulated_list_print_column'.
    !>
    !> If CHANGE_ENTRY_DATA is true, modify the underlying entry data by
    !> setting the appropriate slot of the entry originally used to print
    !> this entry.  Port of tabulated-list.el:tabulated-list-set-col.
    recursive subroutine tabulated_list_set_column(description, status, column, column_name, change_entry_data)
        type(tabulated_list_description_type), intent(in) :: description
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: column
        character(len=*), intent(in), optional :: column_name
        logical, intent(in), optional :: change_entry_data
        type(tabulated_list_column_type), allocatable :: format(:)
        type(value_type) :: v, id, entry_property
        character(len=:), allocatable :: name
        integer(int64) :: original_point, end_of_line, position, next, here
        integer :: c, k, e, x
        logical :: found, ro, change
        type(status_type) :: st

        original_point = point()
        end_of_line = position_end_of_line()
        position = position_beginning_of_line()
        call tabulated_list_format%get_current(format)

        if (present(column)) then
            c = column

            if (c < 1 .or. c > size(format)) then
                call set_error(status, GENERIC_ERROR_CODE, "Invalid column")
                return
            end if

            name = format(c)%name
        else if (present(column_name)) then
            name = column_name
            c = column_number(format, column_name, status)
            if (.not. status%ok()) return
        else
            call set_error(status, GENERIC_ERROR_CODE, "Invalid column")
            return
        end if

        e = entry_index(position)

        if (e == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "No Tabulated List entry at position")
            return
        end if

        call get_text_property(position, value_type(name="tabulated_list_id"), id, st)
        call get_text_property(position, value_type(name="tabulated_list_entry"), entry_property, st)
        call get_text_property(position, value_type(name="tabulated_list_column_name"), v, st)
        found = .false.
        if (vtag(v) == V_STRING) found = same(xstring(v), name)

        do while (.not. found)
            call next_single_property_change(position, value_type(name="tabulated_list_column_name"), next, found, st, end_of_line)
            if (.not. found) next = end_of_line
            position = next
            found = .false.
            if (position >= end_of_line) exit
            call get_text_property(position, value_type(name="tabulated_list_column_name"), v, st)
            if (vtag(v) == V_STRING) found = same(xstring(v), name)
        end do

        status = status_type()
        if (position >= end_of_line) return
        ro = buffers(current)%read_only
        buffers(current)%read_only = .false.
        call next_single_property_change(position, value_type(name="tabulated_list_column_name"), next, found, st, end_of_line)
        if (.not. found) next = end_of_line
        call delete_region(position, next, status)

        if (status%ok()) then
            call goto_character(position)
            k = tabulated_list_entries%get_local_index()
            has_near_rows = .true.
            here = entry_index(position_beginning_of_line(0_int64))

            if (here > 0 .and. position_beginning_of_line(0_int64) < position_beginning_of_line()) then
                near_rows(1) = tabulated_list_entries%locals(k)%entries(here)
            else
                near_rows(1) = tabulated_list_entries%locals(k)%entries(e)
            end if

            near_rows(2) = tabulated_list_entries%locals(k)%entries(e)
            here = entry_index(position_beginning_of_line(2_int64))

            if (here > 0) then
                near_rows(3) = tabulated_list_entries%locals(k)%entries(here)
            else
                near_rows(3) = tabulated_list_entries%locals(k)%entries(e)
            end if

            x = tabulated_list_print_column(c, description, int(current_column()), status)
            has_near_rows = .false.
            change = .false.
            if (present(change_entry_data)) change = change_entry_data
            if (change) tabulated_list_entries%locals(k)%entries(e)%columns(c) = description
            if (status%ok()) call put_text_property(position, point(), value_type(name="tabulated_list_id"), id, status)
            if (status%ok()) call put_text_property(position, point(), value_type(name="tabulated_list_entry"), entry_property, &
                                                    status)
            call goto_character(original_point)
        end if

        buffers(current)%read_only = ro
    end subroutine tabulated_list_set_column

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
        call tabulated_list_format%get_current(format)

        if (present(n)) then
            if (n >= size(format) .or. n < -1) then
                call set_error(status, USER_ERROR_CODE, "Invalid column number")
                return
            end if
        end if

        call make_entries_local(k)

        if (present(n)) then
            if (n == -1) then
                ! Restore original order.
                if (.not. tabulated_list_entries%locals(k)%original_order) then
                    call set_error(status, GENERIC_ERROR_CODE, "Order is already in original order")
                    return
                end if

                call sort_by_original_index(tabulated_list_entries%locals(k)%entries)
                call tabulated_list_sort_key%set_local()
                call tabulated_list_initialize_header()
                call tabulated_list_print(status, remember_position=.true.)
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

        call set_error(status, USER_ERROR_CODE, "Cannot sort by "//name)
    end subroutine tabulated_list_sort

    recursive subroutine execute_tabulated_list_sort(status)
        type(status_type), intent(out) :: status

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call tabulated_list_sort(status)
        else
            call tabulated_list_sort(status, int(prefix_numeric_value(current_prefix_argument)))
        end if
    end subroutine execute_tabulated_list_sort

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
        character(len=:), allocatable :: sort_name
        integer :: k, i
        logical :: flip

        status = status_type()
        if (len(name) == 0 .or. .not. is_derived_mode("tabulated_list_mode")) return
        call make_entries_local(k)

        if (.not. tabulated_list_entries%locals(k)%original_order) then
            ! Store the original order so that we can restore it later.
            tabulated_list_entries%locals(k)%original_order = .true.

            do i = 1, size(tabulated_list_entries%locals(k)%entries)
                tabulated_list_entries%locals(k)%entries(i)%original_index = i
            end do
        end if

        ! Flip the sort order on a second click.
        call tabulated_list_sort_key%get_current(sort_name, flip)

        if (same(sort_name, name)) then
            call tabulated_list_sort_key%set_local(name,.not. flip)
        else
            call tabulated_list_sort_key%set_local(name)
        end if

        call tabulated_list_initialize_header()
        call tabulated_list_print(status, remember_position=.true.)
    end subroutine sort_by_column_name

    !> Widen the current tabulated-list column by N chars.
    !> Interactively, N is the prefix numeric argument, and defaults to 1.
    !> Port of tabulated-list.el:tabulated-list-widen-current-column.
    recursive subroutine tabulated_list_widen_current_column(status, n)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: n
        type(tabulated_list_entry_type) :: entry
        type(tabulated_list_column_type), allocatable :: format(:)
        integer(int64) :: start
        integer :: column_nb, total_width, column_width, by
        logical :: found

        status = status_type()
        by = 1
        if (present(n)) by = n
        start = current_column()
        call tabulated_list_get_entry(entry, found)
        if (.not. found) return
        call tabulated_list_format%get_current(format)
        column_nb = 1
        total_width = 0

        do while (column_nb <= size(format))
            column_width = format(column_nb)%width
            total_width = total_width + max(column_width, int(string_width(entry%columns(column_nb)%text))) + &
                          format(column_nb)%pad_right

            if (start >= total_width) then
                column_nb = column_nb + 1
            else
                format(column_nb)%width = max(1, column_width + by)
                call tabulated_list_format%set_local(format)
                call tabulated_list_print(status, remember_position=.true.)
                call tabulated_list_initialize_header()
                return
            end if
        end do
    end subroutine tabulated_list_widen_current_column

    recursive subroutine execute_tabulated_list_widen_current_column(status)
        type(status_type), intent(out) :: status

        call tabulated_list_widen_current_column(status, int(prefix_numeric_value(current_prefix_argument)))
    end subroutine execute_tabulated_list_widen_current_column

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

    recursive subroutine execute_tabulated_list_narrow_current_column(status)
        type(status_type), intent(out) :: status

        call tabulated_list_narrow_current_column(status, int(prefix_numeric_value(current_prefix_argument)))
    end subroutine execute_tabulated_list_narrow_current_column

    !> Go to the start of the next column after point on the current line.
    !> If ARGUMENT is provided, move that many columns.  Port of
    !> tabulated-list.el:tabulated-list-next-column.
    subroutine tabulated_list_next_column(argument)
        integer, intent(in), optional :: argument
        integer(int64) :: next
        type(status_type) :: st
        logical :: found
        integer :: i, times

        times = 1
        if (present(argument)) times = argument

        do i = 1, times
            call next_single_property_change(point(), value_type(name="tabulated_list_column_name"), next, found, st)
            if (.not. found) next = point_maximum()
            if (next <= position_end_of_line()) call goto_character(next)
        end do
    end subroutine tabulated_list_next_column

    recursive subroutine execute_tabulated_list_next_column(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call tabulated_list_next_column(int(prefix_numeric_value(current_prefix_argument)))
    end subroutine execute_tabulated_list_next_column

    !> Go to the start of the column point is in on the current line.
    !> If ARGUMENT is provided, move that many columns.  Port of
    !> tabulated-list.el:tabulated-list-previous-column.
    subroutine tabulated_list_previous_column(argument)
        integer, intent(in), optional :: argument
        integer(int64) :: previous
        type(status_type) :: st
        logical :: found
        integer :: i, times

        times = 1
        if (present(argument)) times = argument

        do i = 1, times
            call previous_single_property_change(point(), value_type(name="tabulated_list_column_name"), previous, found, st)
            if (.not. found) previous = 1
            if (.not. (previous < position_beginning_of_line())) call goto_character(previous)
        end do
    end subroutine tabulated_list_previous_column

    recursive subroutine execute_tabulated_list_previous_column(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call tabulated_list_previous_column(int(prefix_numeric_value(current_prefix_argument)))
    end subroutine execute_tabulated_list_previous_column

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
    !>  - Call `tabulated_list_initialize_header' to initialize
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
        call tabulated_list_entries%make_local_entry(k)
        if (k > 0) tabulated_list_entries%locals(k)%original_order = .false.
        call major_modes%tabulated_list_mode%end(saved, status)
    end subroutine tabulated_list_mode

    !> Define the mode, its keymap and options, and register the commands.
    !> Not a port.
    subroutine initialize_tabulated_list()
        type(status_type) :: st

        if (.not. allocated(tabulated_list_graphical_sort_indicator_asc)) then
            tabulated_list_graphical_sort_indicator_asc = char(226)//char(150)//char(188)
        end if

        if (.not. allocated(tabulated_list_graphical_sort_indicator_description)) then
            tabulated_list_graphical_sort_indicator_description = char(226)//char(150)//char(178)
        end if

        if (.not. allocated(tabulated_list_terminal_sort_indicator_asc)) tabulated_list_terminal_sort_indicator_asc = "v"

        if (.not. allocated(tabulated_list_terminal_sort_indicator_description)) then
            tabulated_list_terminal_sort_indicator_description = "^"
        end if

        call tabulated_list_fake_header%define("tabulated_list_fake_header", "Face used on fake header lines.")
        call tabulated_list_fake_header%specification_entry(display_when(), face_attributes(overline=.true., underline=.true., &
                                                                                            weight="bold"))
        call append_locals_clearer(clear_tabulated_list_locals)
        call forward_setting(tabulated_list_padding, local_if_set=.true.)
        call forward_setting(tabulated_list_use_header_line, local_if_set=.true.)

        if (.not. associated(tabulated_list_printer%procedure)) then
            tabulated_list_printer%procedure => tabulated_list_print_entry
        end if

        call major_modes%tabulated_list_mode%define("tabulated_list_mode", "Tabulated", tabulated_list_mode, &
                                                    tabulated_list_mode_hook, parent=major_modes%special_mode)
        tabulated_list_mode_map = major_modes%tabulated_list_mode%map
        ! The keys of the other parent, button-buffer-map (no composed keymaps).
        call keymap_set(tabulated_list_mode_map, "TAB", "forward_button", st)
        call keymap_set(tabulated_list_mode_map, "ESC TAB", "backward_button", st)
        call keymap_set(tabulated_list_mode_map, "<backtab>", "backward_button", st)
        call keymap_set(tabulated_list_mode_map, "n", "next_line", st)
        call keymap_set(tabulated_list_mode_map, "p", "previous_line", st)
        call keymap_set(tabulated_list_mode_map, "M-<left>", "tabulated_list_previous_column", st)
        call keymap_set(tabulated_list_mode_map, "M-<right>", "tabulated_list_next_column", st)
        call keymap_set(tabulated_list_mode_map, "S", "tabulated_list_sort", st)
        call keymap_set(tabulated_list_mode_map, "}", "tabulated_list_widen_current_column", st)
        call keymap_set(tabulated_list_mode_map, "{", "tabulated_list_narrow_current_column", st)
        commands%tabulated_list_mode%execute => tabulated_list_mode
        commands%tabulated_list_revert%execute => tabulated_list_revert
        commands%tabulated_list_sort%execute => execute_tabulated_list_sort
        commands%tabulated_list_widen_current_column%execute => execute_tabulated_list_widen_current_column
        commands%tabulated_list_narrow_current_column%execute => execute_tabulated_list_narrow_current_column
        commands%tabulated_list_next_column%execute => execute_tabulated_list_next_column
        commands%tabulated_list_previous_column%execute => execute_tabulated_list_previous_column
    end subroutine initialize_tabulated_list
end module tabulated_list_mod
