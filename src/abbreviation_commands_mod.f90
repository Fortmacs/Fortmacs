! abbreviation_commands_mod.f90 -- the procedures and commands of abbreviation_mod.
! Ported from GNU Emacs 31.1 lisp/abbrev.el.
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

!> The abbrev tables, expansion and the abbrev commands.  A submodule of
!> abbreviation_mod because they use modules (modes, windows, the minibuffer)
!> that use abbreviation_mod.
submodule(abbreviation_mod) abbreviation_commands_mod
    use, intrinsic :: iso_fortran_env, only: int32
    use utility_functions_mod, only: count_characters, is_string_less
    use execute_interactively_mod, only: PREFIX_NONE
    use commands_mod, only: commands
    use error_mod, only: set_error, substitute_quotes, GENERIC_ERROR_CODE
    use data_mod, only: append_locals_clearer, forward_setting
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, erase_buffer, close_buffer, &
                          restore_buffer_modified, generate_new_buffer_name
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use editing_functions_mod, only: interactive_region, point, point_minimum, point_maximum, goto_character, buffer_substring, &
                                     delete_region, &
                                     character_after, line_beginning_position, region_beginning, region_end, is_end_of_buffer, &
                                     point_marker
    use insert_delete_mod, only: insert, replace_range
    use syntax_mod, only: forward_word, skip_syntax_forward, skip_syntax_backward, syntax_table, set_syntax_table, &
                          standard_syntax_table
    use search_mod, only: string_match, match_beginning, match_end, case_fold_search, search_forward, &
                          SEARCH_NOERROR, SEARCH_MOVE
    use basic_subprograms_mod, only: looking_back, read_y_or_n
    use casefiddle_mod, only: downcase_string, upcase_region, upcase_initials_region
    use editing_commands_mod, only: delete_character
    use indent_mod, only: indent_to
    use print_mod, only: fortran_string
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use undo_mod, only: undo_boundary
    use frame_mod, only: selected_frame
    use minibuffer_reading_mod, only: read_string
    use minibuffer_mod, only: read_file_name
    use simple_mod, only: is_use_region, push_mark
    use window_mod, only: display_buffer, switch_to_buffer, same_handle
    use file_input_output_mod, only: file_exists, file_name_directory, expand_file_name, insert_file_contents, write_region, &
                                     is_file_directory
    use files_mod, only: create_directory, append_save_some_buffers_function
    use configuration_mod, only: initialization_file_name
    use names_mod, only: user_initialization_file
    use display_engine_mod, only: message
    use modes_mod, only: major_modes, minor_modes
    implicit none(type, external)

    character(len=*), parameter :: LF = achar(10)

    !> The lines that delimit the abbrevs `write_abbreviation_file' writes into
    !> the init file.
    character(len=*), parameter :: BLOCK_START = "! Abbrevs, written by write_abbreviation_file:"
    character(len=*), parameter :: BLOCK_END = "! End of abbrevs."

contains

    ! ------------------------------------------------------------------
    ! Tables
    ! ------------------------------------------------------------------

    !> Whether A and B are the same string (Fortran's == ignores
    !> trailing blanks).  Not a port.
    pure logical function same_string(a, b)
        character(len=*), intent(in) :: a, b

        same_string = .false.
        if (len(a) /= len(b)) return
        same_string = a == b
    end function same_string

    !> The index of the abbrev named NAME in TABLE (interned upstream), or 0.
    !> Not a port.
    pure integer function find_abbreviation(table, name) result(k)
        type(abbreviation_table_type), intent(in) :: table
        character(len=*), intent(in) :: name
        integer :: i

        k = 0

        do i = 1, table%n
            if (same_string(table%abbreviations(i)%name, name)) then
                k = i
                return
            end if
        end do
    end function find_abbreviation

    !> The index of the abbrev named NAME in TABLE, added undefined if it
    !> is not there (as upstream's obarray-put).  Not a port.
    integer function intern_abbreviation(table, name) result(k)
        type(abbreviation_table_type), intent(inout) :: table
        character(len=*), intent(in) :: name
        type(abbreviation_type), allocatable :: grown(:)

        k = find_abbreviation(table, name)
        if (k > 0) return
        if (.not. allocated(table%abbreviations)) allocate (table%abbreviations(8))

        if (table%n == size(table%abbreviations)) then
            allocate (grown(2*table%n))
            grown(1:table%n) = table%abbreviations(1:table%n)
            call move_alloc(grown, table%abbreviations)
        end if

        table%n = table%n + 1
        k = table%n
        table%abbreviations(k)%name = name
    end function intern_abbreviation

    !> Put the built-in tables in `abbreviation_tables' the first time:
    !> fundamental-mode-abbrev-table and global-abbrev-table.  Not a port.
    subroutine ensure_tables()
        if (allocated(abbreviation_tables)) return
        allocate (abbreviation_tables(2))
        abbreviation_tables(1)%p => fundamental_mode_abbreviation_table
        abbreviation_tables(2)%p => global_abbreviation_table
    end subroutine ensure_tables

    module procedure abbreviation_table_define
    integer :: k, system_flag
    logical :: unchanged

    if (present(system)) then
        system_flag = merge(1, 0, system)
    else
        system_flag = 0
    end if

    if (present(force)) then
        if (force) system_flag = 2
    end if

    k = intern_abbreviation(self, abbreviation)
    ! Don't override a prior user-defined abbrev with a system abbrev,
    ! unless system-flag is `force'.
    if (system_flag == 1 .and. self%abbreviations(k)%bound .and. allocated(self%abbreviations(k)%expansion) .and. &
        .not. self%abbreviations(k)%system) return

    if (system_flag == 0) then
        unchanged = self%abbreviations(k)%bound

        if (unchanged) then
            if (present(expansion)) then
                unchanged = .false.
                if (allocated(self%abbreviations(k)%expansion)) unchanged = same_string(self%abbreviations(k)%expansion, expansion)
            else
                unchanged = .not. allocated(self%abbreviations(k)%expansion)
            end if
        end if

        if (unchanged) then
            if (present(hook)) then
                unchanged = associated(self%abbreviations(k)%hook, hook)
            else
                unchanged = .not. associated(self%abbreviations(k)%hook)
            end if
        end if

        if (.not. unchanged) abbreviations_changed = .true.
    end if

    if (present(expansion)) then
        self%abbreviations(k)%expansion = expansion
    else if (allocated(self%abbreviations(k)%expansion)) then
        deallocate (self%abbreviations(k)%expansion)
    end if

    self%abbreviations(k)%hook => null()
    if (present(hook)) self%abbreviations(k)%hook => hook
    self%abbreviations(k)%bound = .true.
    ! The properties are replaced (setplist).
    self%abbreviations(k)%count = 0
    if (present(count)) self%abbreviations(k)%count = count
    ! Don't store the `force' value of `system-flag' into the :system
    ! property.
    self%abbreviations(k)%system = system_flag /= 0
    self%abbreviations(k)%case_fixed = .false.
    if (present(case_fixed)) self%abbreviations(k)%case_fixed = case_fixed
    self%abbreviations(k)%no_self_insert = .false.
    if (present(no_self_insert)) self%abbreviations(k)%no_self_insert = no_self_insert
    self%abbreviations(k)%enable_function => null()
    if (present(enable_function)) self%abbreviations(k)%enable_function => enable_function
    self%modiff = self%modiff + 1
    end procedure abbreviation_table_define

    module procedure abbreviation_table_define_table
    integer :: k
    type(abbreviation_table_type), pointer :: table

    call ensure_tables()
    self%name = name
    table => null()

    select type (self)
    type is (abbreviation_table_type)
        table => self
    end select

    if (.not. associated(table)) return

    do k = 1, size(abbreviation_tables)
        if (associated(abbreviation_tables(k)%p, table)) return
    end do

    abbreviation_tables = [abbreviation_table_reference_type(table), abbreviation_tables]
    end procedure abbreviation_table_define_table

    module procedure clear_abbreviation_table
    abbreviations_changed = .true.
    if (allocated(self%abbreviations)) deallocate (self%abbreviations)
    self%n = 0
    ! Preserve the table's properties.
    self%modiff = self%modiff + 1
    end procedure clear_abbreviation_table

    module procedure is_abbreviation_table_empty
    integer :: k
    logical :: ignore

    ignore = .false.
    if (present(ignore_system)) ignore = ignore_system
    empty = .true.

    do k = 1, self%n
        if (len(self%abbreviations(k)%name) == 0) cycle
        if (ignore .and. self%abbreviations(k)%system) cycle
        empty = .false.
        return
    end do
    end procedure is_abbreviation_table_empty

    module procedure abbreviation_table_append_parent
    if (.not. allocated(self%parents)) allocate (self%parents(0))
    self%parents = [self%parents, abbreviation_table_reference_type(parent)]
    end procedure abbreviation_table_append_parent

    module procedure clear_all_abbreviations
    integer :: k

    call ensure_tables()

    do k = 1, size(abbreviation_tables)
        call abbreviation_tables(k)%p%clear()
    end do
    end procedure clear_all_abbreviations

    module procedure copy_abbreviation_table
    integer :: k

    do k = 1, table%n
        associate (a => table%abbreviations(k))
            if (allocated(a%expansion)) then
                if (associated(a%hook)) then
                    call new%define(a%name, a%expansion, hook=a%hook)
                else
                    call new%define(a%name, a%expansion)
                end if
            else if (associated(a%hook)) then
                call new%define(a%name, hook=a%hook)
            else
                call new%define(a%name)
            end if
        end associate
    end do
    end procedure copy_abbreviation_table

    module procedure abbreviation_table_name
    integer :: k

    call ensure_tables()
    name = ""

    do k = 1, size(abbreviation_tables)
        if (associated(abbreviation_tables(k)%p, table)) then
            name = trim(abbreviation_tables(k)%p%name)
            return
        end if
    end do
    end procedure abbreviation_table_name

    module procedure abbreviation_table_names
    integer :: k, m
    logical :: include_system
    integer :: width

    include_system = .false.
    if (present(system)) include_system = system
    width = 1
    m = 0

    do k = 1, table%n
        if (.not. user_abbreviation(k)) cycle
        m = m + 1
        width = max(width, len(table%abbreviations(k)%name))
    end do

    allocate (character(len=width) :: names(m))
    m = 0

    do k = 1, table%n
        if (.not. user_abbreviation(k)) cycle
        m = m + 1
        names(m) = table%abbreviations(k)%name
    end do

contains

    logical function user_abbreviation(i)
        integer, intent(in) :: i

        user_abbreviation = allocated(table%abbreviations(i)%expansion)
        if (user_abbreviation .and. .not. include_system) user_abbreviation = .not. table%abbreviations(i)%system
    end function user_abbreviation
    end procedure abbreviation_table_names

    !> The abbrevs of TABLE that `abbreviation_table_names' gives (with SYSTEM),
    !> sorted by name (string-lessp), as indices.  Not a port.
    subroutine sorted_abbreviations(table, system, order)
        type(abbreviation_table_type), intent(in) :: table
        logical, intent(in) :: system
        integer, allocatable, intent(out) :: order(:)
        integer :: k, m, j, x

        allocate (order(table%n))
        m = 0

        do k = 1, table%n
            if (.not. allocated(table%abbreviations(k)%expansion)) cycle
            if (.not. system .and. table%abbreviations(k)%system) cycle
            m = m + 1
            order(m) = k
        end do

        order = order(1:m)

        ! Insertion sort (few abbrevs).
        do k = 2, m
            x = order(k)
            j = k - 1

            do while (j >= 1)
                if (.not. is_string_less(table%abbreviations(x)%name, table%abbreviations(order(j))%name)) exit
                order(j + 1) = order(j)
                j = j - 1
            end do

            order(j + 1) = x
        end do
    end subroutine sorted_abbreviations

    ! ------------------------------------------------------------------
    ! Local tables (buffer.c's BVAR abbreviation_table)
    ! ------------------------------------------------------------------

    module procedure set_local_abbreviation_table_local
    integer :: k

    call self%make_local_entry(k)
    if (k > 0) self%locals(k)%table => table
    end procedure set_local_abbreviation_table_local

    module procedure get_local_abbreviation_table_current
    integer :: k

    table => self%table
    k = self%get_local_index()
    if (k > 0) table => self%locals(k)%table
    if (.not. associated(table)) table => fundamental_mode_abbreviation_table
    end procedure get_local_abbreviation_table_current

    module procedure append_local_abbreviation_table_entry
    type(local_abbreviation_table_entry_type) :: entry

    if (.not. allocated(self%locals)) allocate (self%locals(0))
    self%locals = [self%locals, entry]
    end procedure append_local_abbreviation_table_entry

    module procedure clear_local_abbreviation_table_entry
    self%locals(k)%table => null()
    end procedure clear_local_abbreviation_table_entry

    ! kill-all-local-variables: the current buffer uses the global
    ! `local_abbreviation_table' again.  Not a port.
    subroutine clear_abbreviation_locals()
        call local_abbreviation_table%clear_local()
    end subroutine clear_abbreviation_locals

    module procedure abbreviation_no_self_insert
    integer :: k

    abbreviation_no_self_insert = .false.
    if (.not. associated(abbreviation%table)) return
    k = find_abbreviation(abbreviation%table, abbreviation%name)
    if (k == 0) return
    abbreviation_no_self_insert = associated(abbreviation%table%abbreviations(k)%hook) .and. abbreviation%table%abbreviations( &
                                  k)%no_self_insert
    end procedure abbreviation_no_self_insert

    module procedure is_abbreviation_mode
    is_abbreviation_mode = .false.
    if (associated(abbreviation_mode)) is_abbreviation_mode = abbreviation_mode%value
    end procedure is_abbreviation_mode

    ! ------------------------------------------------------------------
    ! Looking up abbrevs
    ! ------------------------------------------------------------------

    !> The list of abbrev tables that are currently active, or TABLE.
    !> Port of abbrev.el:abbrev--active-tables (no minor-mode tables).
    subroutine active_tables(tables, table)
        type(abbreviation_table_reference_type), allocatable, intent(out) :: tables(:)
        type(abbreviation_table_type), intent(in), target, optional :: table

        if (present(table)) then
            allocate (tables(1))
            tables(1)%p => table
            return
        end if

        allocate (tables(2))
        tables(1)%p => local_abbreviation_table%get_current()
        tables(2)%p => global_abbreviation_table
    end subroutine active_tables

    !> Take the first table off QUEUE into TABLE and put its parents in
    !> front of the rest.  Not a port.
    subroutine pop_table(queue, table)
        type(abbreviation_table_reference_type), allocatable, intent(inout) :: queue(:)
        type(abbreviation_table_type), pointer, intent(out) :: table
        type(abbreviation_table_reference_type), allocatable :: rest(:)
        integer :: np

        table => queue(1)%p
        np = 0
        if (allocated(table%parents)) np = size(table%parents)
        allocate (rest(np + size(queue) - 1))
        if (np > 0) rest(1:np) = table%parents
        rest(np + 1:) = queue(2:)
        call move_alloc(rest, queue)
    end subroutine pop_table

    !> The index of the abbrev named ABBREVIATION in TABLE, or 0 if such an
    !> abbrev is not defined.  Port of abbrev.el:abbrev--symbol.
    integer function table_name(abbreviation, table) result(k)
        character(len=*), intent(in) :: abbreviation
        type(abbreviation_table_type), intent(in) :: table
        integer :: j

        ! First try without case-folding.
        k = find_abbreviation(table, abbreviation)

        if (k == 0 .and. .not. table%case_fixed) then
            ! We didn't find any abbrev, try case-folding.
            j = find_abbreviation(table, downcase_string(abbreviation))

            ! Only use it if it doesn't require :case-fixed.
            if (j > 0) then
                if (.not. table%abbreviations(j)%case_fixed) k = j
            end if
        end if

        if (k > 0) then
            if (.not. allocated(table%abbreviations(k)%expansion)) k = 0
        end if
    end function table_name

    !> abbrev-symbol over the tables TABLES.
    !> Port of abbrev.el:abbrev-symbol, in part.
    subroutine name_in(abbreviation, tables, result)
        character(len=*), intent(in) :: abbreviation
        type(abbreviation_table_reference_type), allocatable, intent(inout) :: tables(:)
        type(abbreviation_reference_type), intent(out) :: result
        type(abbreviation_table_type), pointer :: table
        integer :: k

        do while (size(tables) > 0)
            call pop_table(tables, table)
            k = table_name(abbreviation, table)

            if (k > 0) then
                result%table => table
                result%name = table%abbreviations(k)%name
                return
            end if
        end do
    end subroutine name_in

    module procedure abbreviation_name
    type(abbreviation_table_reference_type), allocatable :: tables(:)

    call active_tables(tables, table)
    call name_in(abbreviation, tables, result)
    end procedure abbreviation_name

    module procedure abbreviation_expansion
    type(abbreviation_reference_type) :: symbol
    integer :: k

    call abbreviation_name(abbreviation, symbol, table)
    if (.not. associated(symbol%table)) return
    k = find_abbreviation(symbol%table, symbol%name)

    if (k > 0) then
        if (allocated(symbol%table%abbreviations(k)%expansion)) expansion = symbol%table%abbreviations(k)%expansion
    end if
    end procedure abbreviation_expansion

    ! ------------------------------------------------------------------
    ! Expanding
    ! ------------------------------------------------------------------

    !> Forget `abbreviation_start_location' (freeing its marker).  Not a port.
    subroutine clear_start_location()
        type(status_type) :: st

        if (abbreviation_start_location%index /= 0) call destroy_marker(abbreviation_start_location, st)
        abbreviation_start_location = handle_type()
    end subroutine clear_start_location

    !> Try and find an abbrev before point: SYMBOL (no table if none), the
    !> NAME it was found by, and its WORDSTART and WORDEND.  Port of
    !> abbrev.el:abbrev--before-point.
    recursive subroutine before_point(symbol, name, wordstart, wordend, status)
        type(abbreviation_reference_type), intent(out) :: symbol
        character(len=:), allocatable, intent(out) :: name
        integer(int64), intent(out) :: wordstart, wordend
        type(status_type), intent(out) :: status
        type(abbreviation_reference_type) :: found
        type(abbreviation_table_reference_type), allocatable :: tables(:)
        type(abbreviation_table_type), pointer :: table
        integer(int64) :: position, start, end, limit_position, moved
        integer(int32) :: c
        logical :: have, enabled
        integer :: k
        type(status_type) :: st

        status = status_type()
        wordstart = 0
        wordend = 0
        name = ""
        if (.not. same_handle(abbreviation_start_location_buffer, current_buffer())) call clear_start_location()
        call active_tables(tables)
        position = point()

        if (abbreviation_start_location%index /= 0) then
            call marker_position(abbreviation_start_location, start, st)
            call clear_start_location()

            ! Remove the hyphen inserted by `abbrev-prefix-mark'.
            if (start < point_maximum()) then
                call character_after(c, have, start)

                if (have .and. c == iachar("-")) then
                    call delete_region(start, start + 1, status)
                    if (.not. status%ok()) return
                    position = position - 1
                end if
            end if

            moved = skip_syntax_backward(" ")
            end = point()

            if (end > start) then
                call buffer_substring(start, end, name, status)
                if (.not. status%ok()) return
                ! Restore point.
                call goto_character(position)
                call name_in(name, tables, symbol)
                wordstart = start
                wordend = end
            end if

            return
        end if

        do while (size(tables) > 0)
            if (associated(found%table)) exit
            call pop_table(tables, table)
            enabled = .true.
            if (associated(table%enable_function)) enabled = table%enable_function()
            have = .false.

            if (enabled) then
                if (.not. allocated(table%regular_expression)) then
                    ! We used to default `regular_expression' to "\\<\\(\\w+\\)\\W*"
                    ! but when words-include-escapes is set, that
                    ! is not right and fixing it is boring.
                    limit_position = point()
                    call forward_word(-1_int64)
                    start = point()
                    call forward_word(1_int64)
                    end = min(point(), limit_position)
                    have = .true.
                else if (looking_back(table%regular_expression, status, limit=line_beginning_position())) then
                    start = match_beginning(1, st)
                    end = match_end(1, st)
                    have = .true.
                end if

                if (.not. status%ok()) return
            end if

            if (have) then
                call buffer_substring(start, end, name, status)
                if (.not. status%ok()) return
                k = table_name(name, table)

                if (k > 0) then
                    enabled = .true.
                    if (associated(table%abbreviations(k)%enable_function)) enabled = table%abbreviations(k)%enable_function()

                    if (enabled) then
                        ! This will also look it up in parent tables.
                        ! This is not on purpose, but it seems harmless.
                        found%table => table
                        found%name = table%abbreviations(k)%name
                        wordstart = start
                        wordend = end
                    end if
                end if
            end if

            ! Restore point.
            call goto_character(position)
        end do

        if (associated(found%table)) symbol = found
    end subroutine before_point

    module procedure abbreviation_insert
    character(len=:), allocatable :: nm, expansion
    integer(int64) :: wstart, wend, p0, a, b, end, moved
    integer :: k
    logical :: saved_fold, value, upper, lower
    procedure(abbreviation_hook_procedure), pointer :: hook
    type(status_type) :: st

    status = status_type()
    if (.not. associated(abbreviation%table)) return
    k = find_abbreviation(abbreviation%table, abbreviation%name)
    if (k == 0) return
    nm = abbreviation%name
    if (present(name)) nm = name
    wstart = point()
    if (present(wordstart)) wstart = wordstart
    wend = wstart
    if (present(wordend)) wend = wordend
    ! Increment use count.
    abbreviation%table%abbreviations(k)%count = abbreviation%table%abbreviations(k)%count + 1
    result = abbreviation

    ! If this abbrev has an expansion, delete the abbrev
    ! and insert the expansion.
    if (allocated(abbreviation%table%abbreviations(k)%expansion)) then
        expansion = abbreviation%table%abbreviations(k)%expansion
        call goto_character(wstart)
        ! Insert at beginning so that markers at the end (e.g. point)
        ! are preserved.
        call insert(expansion, status)
        if (.not. status%ok()) return
        call delete_character(wend - wstart, status)
        if (.not. status%ok()) return
        saved_fold = case_fold_search%value
        case_fold_search = .false.
        ! If the abbrev's name is different from the buffer text (the
        ! only difference should be capitalization), then we may want
        ! to adjust the capitalization of the expansion.
        upper = .false.
        if (.not. same_string(nm, abbreviation%name)) upper = string_match("[[:upper:]]", nm, st) > 0

        if (upper) then
            lower = string_match("[[:lower:]]", nm, st) > 0

            if (.not. lower) then
                ! Abbrev was all caps.  If expansion is multiple words,
                ! normally capitalize each word.
                a = 0
                b = 0

                if (.not. abbreviation_all_caps%value) then
                    p0 = point()
                    call forward_word(-1_int64)
                    a = point()
                    call goto_character(wstart)
                    call forward_word(1_int64)
                    b = point()
                    call goto_character(p0)
                end if

                if (.not. abbreviation_all_caps%value .and. a > b) then
                    call upcase_initials_region(wstart, point(), status)
                else
                    call upcase_region(wstart, point(), status)
                end if
            else
                ! Abbrev included some caps.  Cap first initial of expansion.
                end = point()
                ! Find the initial.
                call goto_character(wstart)
                moved = skip_syntax_forward("^w", end - 1)
                ! Change just that.
                call upcase_initials_region(point(), point() + 1, status)
                call goto_character(end)
            end if
        end if

        case_fold_search = saved_fold
        if (.not. status%ok()) return
    end if

    ! Now point is at the end of the expansion and the beginning is
    ! in last-abbrev-location.
    hook => abbreviation%table%abbreviations(k)%hook

    if (associated(hook)) then
        ! If the abbrev has a hook function, run it.
        call hook(value, status)
        if (.not. status%ok()) return
        ! In addition, if the hook function has the `no_self_insert'
        ! property, let the value it returned specify whether we consider
        ! that an expansion took place.  If it returns false, no expansion
        ! has been done.
        k = find_abbreviation(abbreviation%table, abbreviation%name)

        if (k > 0) then
            if (.not. value .and. abbreviation%table%abbreviations(k)%no_self_insert) then
                result%table => null()
                if (allocated(result%name)) deallocate (result%name)
            end if
        end if
    end if
    end procedure abbreviation_insert

    module procedure expand_abbreviation
    type(abbreviation_reference_type) :: symbol

    if (associated(abbreviation_expand_function)) then
        call abbreviation_expand_function(symbol, status)
    else
        call abbreviation_default_expand(symbol, status)
    end if

    if (present(expanded)) expanded = symbol
    end procedure expand_abbreviation

    module procedure abbreviation_default_expand
    type(abbreviation_reference_type) :: symbol
    character(len=:), allocatable :: name
    integer(int64) :: wordstart, wordend, sp, ep
    type(handle_type) :: startpos, endmark
    type(status_type) :: st

    call before_point(symbol, name, wordstart, wordend, status)
    if (.not. status%ok()) return
    if (.not. associated(symbol%table)) return
    call copy_marker_at(point(), startpos, .true., st)
    call copy_marker_at(wordend, endmark, .true., st)
    ! Add an undo boundary, in case we are doing this for a
    ! self-inserting command which has avoided making one so far.
    if (.not. selected_frame%minibuffer_selected) call undo_boundary()
    ! Now sym is the abbrev symbol.
    last_abbreviation_text = name
    last_abbreviation = symbol
    last_abbreviation_location = wordstart
    ! If this abbrev has an expansion, delete the abbrev
    ! and insert the expansion.
    call abbreviation_insert(symbol, abbreviation, status, name, wordstart, wordend)
    ! Yuck!!  If expand-abbrev is called with point slightly
    ! further than the end of the abbrev, move point back to
    ! where it started.
    call marker_position(startpos, sp, st)
    call marker_position(endmark, ep, st)
    if (status%ok() .and. sp > ep .and. point() == ep) call goto_character(sp)
    call destroy_marker(startpos, st)
    call destroy_marker(endmark, st)
    end procedure abbreviation_default_expand

    module procedure unexpand_abbreviation
    integer(int64) :: position, vlen, tlen
    type(handle_type) :: saved
    type(status_type) :: st
    integer :: k
    character(len=:), allocatable :: value

    status = status_type()
    ! (save-excursion ...)
    call point_marker(saved)

    if (point_minimum() <= last_abbreviation_location .and. last_abbreviation_location <= point_maximum()) then
        call goto_character(last_abbreviation_location)

        if (allocated(last_abbreviation_text)) then
            ! This isn't correct if last-abbrev's hook was used
            ! to do the expansion.
            k = 0
            if (associated(last_abbreviation%table)) k = find_abbreviation(last_abbreviation%table, last_abbreviation%name)

            if (k > 0) then
                if (allocated(last_abbreviation%table%abbreviations(k)%expansion)) value = last_abbreviation%table%abbreviations( &
                                                                                           k)%expansion
            end if

            if (.not. allocated(value)) then
                call set_error(status, GENERIC_ERROR_CODE, "Value of abbreviation_name must be a string")
            else
                vlen = count_characters(value)
                tlen = count_characters(last_abbreviation_text)

                if (point() + vlen > point_maximum()) then
                    ! replace-region-contents validates the region.
                    call delete_region(point(), point() + vlen, status)
                else
                    ! Don't inherit properties here; just copy from old contents.
                    position = point()
                    call replace_range(position, position + vlen, last_abbreviation_text, .true., status)

                    if (status%ok()) then
                        call goto_character(position + tlen)
                        deallocate (last_abbreviation_text)
                    end if
                end if
            end if
        end if
    end if

    call marker_position(saved, position, st)
    call goto_character(position)
    call destroy_marker(saved, st)
    end procedure unexpand_abbreviation

    ! ------------------------------------------------------------------
    ! Describing tables
    ! ------------------------------------------------------------------

    !> Describe abbrev K of TABLE.
    !> Print on the current buffer the abbrev, count of use, expansion.
    !> Port of abbrev.el:abbrev--describe (the hook is not shown).
    recursive subroutine describe_abbreviation(table, k, status)
        type(abbreviation_table_type), intent(in) :: table
        integer, intent(in) :: k
        type(status_type), intent(out) :: status
        integer(int64) :: column
        character(len=24) :: buffer

        status = status_type()

        associate (a => table%abbreviations(k))
            if (.not. allocated(a%expansion)) return
            call insert(fortran_string(a%name), status)
            if (.not. status%ok()) return

            if (.not. a%system) then
                column = indent_to(15_int64, status, 1_int64)
            else
                call insert(" (sys)", status)
                if (status%ok()) column = indent_to(20_int64, status, 1_int64)
            end if

            if (.not. status%ok()) return
            write (buffer, "(i0)") a%count
            call insert(trim(buffer), status)
            if (status%ok()) column = indent_to(20_int64, status, 1_int64)
            if (status%ok()) call insert(fortran_string(a%expansion)//LF, status)
        end associate
    end subroutine describe_abbreviation

    !> STRING as Fortran source: a literal in pieces of at most 60
    !> bytes joined by //, control characters as achar(N), the pieces
    !> continued over lines indented by IND.  Not a port.
    function fortran_literal(string, ind) result(s)
        character(len=*), intent(in) :: string, ind
        character(len=:), allocatable :: s, piece
        integer :: k, b
        character(len=8) :: buffer

        s = ""
        piece = ""
        k = 1

        do while (k <= len(string))
            b = ichar(string(k:k))

            if (b < 32 .or. b == 127) then
                if (len(piece) > 0) s = joined(s, fortran_string(piece))
                piece = ""
                write (buffer, "(i0)") b
                s = joined(s, "achar("//trim(buffer)//")")
                k = k + 1
                cycle
            end if

            ! Break between characters, not inside one.
            if (len(piece) >= 60 .and. (b < 128 .or. b >= 192)) then
                s = joined(s, fortran_string(piece))
                piece = ""
            end if

            piece = piece//string(k:k)
            k = k + 1
        end do

        if (len(piece) > 0 .or. len(s) == 0) s = joined(s, fortran_string(piece))

    contains

        function joined(left, right) result(r)
            character(len=*), intent(in) :: left, right
            character(len=:), allocatable :: r

            if (len(left) == 0) then
                r = right
            else
                r = left//"// &"//LF//ind//right
            end if
        end function joined
    end function fortran_literal

    !> The Fortran statement defining abbrev K of TABLE in `configure'
    !> (upstream's abbrev--write).
    !> Port of abbrev.el:abbrev--write (a Fortran statement).
    function define_statement(table, k) result(s)
        type(abbreviation_table_type), intent(in) :: table
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        character(len=24) :: buffer
        character(len=*), parameter :: IND = "            "

        associate (a => table%abbreviations(k))
            write (buffer, "(i0)") a%count
            s = "        call "//trim(table%name)//"%define("//fortran_literal(a%name, IND)//", &"//LF//IND// &
                fortran_literal(a%expansion, IND)//", count="//trim(buffer)//"_int64"
            if (a%case_fixed) s = s//", case_fixed=.true."
            s = s//")"//LF
        end associate
    end function define_statement

    module procedure insert_abbreviation_table_description
    integer, allocatable :: order(:)
    logical :: human
    integer :: k

    human = .false.
    if (present(readable)) human = readable
    call sorted_abbreviations(table, human, order)

    if (human) then
        call insert("("//trim(table%name)//")"//LF//LF, status)

        do k = 1, size(order)
            if (.not. status%ok()) return
            call describe_abbreviation(table, order(k), status)
        end do

        if (status%ok()) call insert(LF//LF, status)
    else
        status = status_type()

        do k = 1, size(order)
            ! An abbrev with a hook or enable function is defined in
            ! code: its procedure has no name to write.
            if (associated(table%abbreviations(order(k))%hook)) cycle
            if (associated(table%abbreviations(order(k))%enable_function)) cycle
            call insert(define_statement(table, order(k)), status)
            if (.not. status%ok()) return
        end do
    end if
    end procedure insert_abbreviation_table_description

    module procedure insert_abbreviations
    integer(int64) :: original_point, position
    integer :: k
    type(handle_type) :: saved
    type(status_type) :: st

    call ensure_tables()
    status = status_type()
    original_point = point()
    call copy_marker_at(original_point, saved, .false., st)

    do k = 1, size(abbreviation_tables)
        call insert_abbreviation_table_description(abbreviation_tables(k)%p, status, readable=.true.)
        if (.not. status%ok()) exit
    end do

    position = point()
    call marker_position(saved, original_point, st)
    call destroy_marker(saved, st)
    call goto_character(original_point)
    if (status%ok()) call push_mark(position)
    end procedure insert_abbreviations

    module procedure prepare_abbreviation_list_buffer
    type(abbreviation_table_type), pointer :: local_table
    type(handle_type) :: old
    type(status_type) :: st
    logical :: only_local
    integer :: k

    call ensure_tables()
    only_local = .false.
    if (present(local)) only_local = local
    local_table => local_abbreviation_table%get_current()
    old = current_buffer()
    call get_or_make_buffer("*Abbrevs*", buffer, status)
    if (.not. status%ok()) return
    call set_buffer(buffer, status)
    if (.not. status%ok()) return
    call erase_buffer(status)

    if (status%ok()) then
        if (only_local) then
            if (len(abbreviation_table_name(local_table)) > 0) then
                call insert_abbreviation_table_description(local_table, status, readable=.true.)
            end if
        else
            do k = 1, size(abbreviation_tables)
                if (.not. abbreviation_tables(k)%p%empty()) then
                    call insert_abbreviation_table_description(abbreviation_tables(k)%p, status, readable=.true.)
                end if

                if (.not. status%ok()) exit
            end do

            do k = 1, size(abbreviation_tables)
                if (.not. status%ok()) exit

                if (abbreviation_tables(k)%p%empty()) then
                    call insert_abbreviation_table_description(abbreviation_tables(k)%p, status, readable=.true.)
                end if
            end do

            ! Note: `list_abbreviations' can display only local abbrevs, in
            ! which case editing could lose abbrevs of other tables.
            ! Thus enter `edit_abbreviations_mode' only if LOCAL is false.
            if (status%ok()) call edit_abbreviations_mode(status)
        end if
    end if

    call goto_character(point_minimum())
    call restore_buffer_modified(.false.)
    call set_buffer(old, st)
    end procedure prepare_abbreviation_list_buffer

    module procedure list_abbreviations
    type(handle_type) :: buffer, window

    call prepare_abbreviation_list_buffer(buffer, status, local)
    if (status%ok()) call display_buffer(buffer, window, status)
    end procedure list_abbreviations

    module procedure edit_abbreviations
    character(len=:), allocatable :: local_name
    type(handle_type) :: buffer
    integer(int64) :: found
    type(status_type) :: st
    type(abbreviation_table_type), pointer :: local_table

    local_table => local_abbreviation_table%get_current()
    local_name = abbreviation_table_name(local_table)
    call prepare_abbreviation_list_buffer(buffer, status)
    if (.not. status%ok()) return
    call switch_to_buffer("*Abbrevs*", status)
    if (.not. status%ok()) return

    if (len(local_name) > 0) then
        call search_forward("("//local_name//")"//LF//LF, found, st, noerror=SEARCH_NOERROR)
        if (found > 0) call goto_character(match_end(0, st))
    end if
    end procedure edit_abbreviations

    ! ------------------------------------------------------------------
    ! Saving abbrevs: the init file
    ! ------------------------------------------------------------------

    !> `abbreviation_file_name', or the init file when it is not set.  Not a port.
    function abbreviation_file() result(file)
        character(len=:), allocatable :: file

        if (allocated(abbreviation_file_name)) then
            file = abbreviation_file_name
        else if (allocated(user_initialization_file)) then
            file = user_initialization_file
            if (len(file) == 0) file = initialization_file_name()
        else
            file = initialization_file_name()
        end if
    end function abbreviation_file

    module procedure write_abbreviation_file
    character(len=:), allocatable :: target_file, block, message_text, directory
    type(abbreviation_table_reference_type), allocatable :: sorted(:)
    type(abbreviation_table_reference_type) :: x
    character(len=:), allocatable :: names(:)
    type(handle_type) :: old, temporary
    type(status_type) :: st
    integer(int64) :: inserted, found, beginning
    integer :: k, j, m
    logical :: saved_fold

    target_file = ""
    if (present(file)) target_file = file
    if (len(target_file) == 0) target_file = abbreviation_file()
    call ensure_tables()
    ! We sort the table in order to ease the automatic
    ! merging of different versions of the user's abbrevs
    ! file.  This is useful, for example, when the
    ! user keeps their home directory in a revision
    ! control system, and therefore keeps multiple
    ! slightly-differing loosely synchronized copies.
    sorted = abbreviation_tables
    m = size(sorted)

    do k = 2, m
        x = sorted(k)
        j = k - 1

        do while (j >= 1)
            if (.not. is_string_less(trim(x%p%name), trim(sorted(j)%p%name))) exit
            sorted(j + 1) = sorted(j)
            j = j - 1
        end do

        sorted(j + 1) = x
    end do

    old = current_buffer()
    call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, status, inhibit_buffer_hooks=.true.)
    if (.not. status%ok()) return
    call set_buffer(temporary, status)
    if (.not. status%ok()) return
    ! The abbrevs, as `define' calls.
    call insert("        "//BLOCK_START//LF, status)

    do k = 1, m
        if (.not. status%ok()) exit
        call abbreviation_table_names(sorted(k)%p, names)
        if (size(names) > 0) call insert_abbreviation_table_description(sorted(k)%p, status)
    end do

    if (status%ok()) call insert("        "//BLOCK_END//LF, status)
    if (status%ok()) call buffer_substring(point_minimum(), point_maximum(), block, status)
    if (status%ok()) call erase_buffer(status)
    ! The init file, with the abbrevs replacing those written before at the
    ! end of `configure'.
    if (status%ok() .and. file_exists(target_file)) call insert_file_contents(target_file, .false., inserted, status)

    if (status%ok() .and. point_maximum() == point_minimum()) then
        ! A new init file.
        call insert("module user_configuration"//LF//"    use fortmacs"//LF//"    implicit none(type, external)"//LF// &
                    "    private"//LF//LF//"    public :: configure"//LF//LF//"contains"//LF//LF// &
                    "    subroutine configure(status)"//LF//"        type(status_type), intent(out) :: status"// &
                    LF//"    end subroutine configure"//LF//"end module user_configuration"//LF, status)
    end if

    saved_fold = case_fold_search%value
    case_fold_search = .true.

    if (status%ok()) then
        call goto_character(point_minimum())
        call search_forward(BLOCK_START, found, st, noerror=SEARCH_NOERROR)

        if (found > 0) then
            beginning = line_beginning_position()
            call search_forward(BLOCK_END, found, st, noerror=SEARCH_NOERROR)

            if (found > 0) then
                call search_forward(LF, found, st, noerror=SEARCH_MOVE)
                call delete_region(beginning, point(), status)
                call goto_character(beginning)
            else
                found = 0
                call goto_character(point_minimum())
            end if
        end if

        if (found == 0) then
            call search_forward("end subroutine configure", found, st, noerror=SEARCH_NOERROR)

            if (found == 0) then
                call set_error(status, GENERIC_ERROR_CODE, substitute_quotes("No `end subroutine configure' in ")//target_file)
            else
                call goto_character(line_beginning_position())
            end if
        end if
    end if

    case_fold_search = saved_fold
    if (status%ok()) call insert(block, status)

    if (status%ok()) then
        directory = file_name_directory(target_file)

        if (len(directory) > 0) then
            if (.not. is_file_directory(directory)) call create_directory(directory, status, parents=.true.)
        end if
    end if

    if (status%ok()) call write_region(target_file, .false., status, message=message_text)
    call set_buffer(old, st)
    call close_buffer(temporary, st)
    if (.not. status%ok()) return

    if (present(verbose)) then
        if (verbose .and. allocated(message_text)) call message(message_text)
    end if
    end procedure write_abbreviation_file

    module procedure abbreviation_possibly_save
    logical :: save

    status = status_type()

    ! Query mode.
    if (query) then
        result = .not. save_abbreviations%is_off() .and. abbreviations_changed
        return
    end if

    result = .false.
    if (save_abbreviations%is_off() .or. .not. abbreviations_changed) return

    if (argument .or. save_abbreviations%choice == SAVE_ABBREVIATIONS_SILENTLY) then
        save = .true.
    else
        call read_y_or_n("Save abbrevs in "//abbreviation_file()//"? ", save, status)
        if (.not. status%ok()) return
    end if

    if (save) then
        call write_abbreviation_file(status)
        if (.not. status%ok()) return
        result = .false.
    else
        ! Inhibit message in `save-some-buffers'.
        result = .true.
    end if

    ! Don't ask again whether saved or user said no.
    abbreviations_changed = .false.
    end procedure abbreviation_possibly_save

    ! ------------------------------------------------------------------
    ! Defining abbrevs from the buffer
    ! ------------------------------------------------------------------

    !> The table the mode-specific commands define abbrevs in:
    !> `global_abbreviation_table' if `only_global_abbreviations', else the local table.
    !> Not a port.
    function mode_table() result(table)
        type(abbreviation_table_type), pointer :: table

        if (only_global_abbreviations) then
            table => global_abbreviation_table
        else
            table => local_abbreviation_table%get_current()
        end if
    end function mode_table

    module procedure add_mode_abbreviation
    type(abbreviation_table_type), pointer :: table

    table => mode_table()
    call add_abbreviation(table, "Mode", argument, status)
    end procedure add_mode_abbreviation

    module procedure add_global_abbreviation
    call add_abbreviation(global_abbreviation_table, "Global", argument, status)
    end procedure add_global_abbreviation

    module procedure add_abbreviation
    character(len=:), allocatable :: exp, name, old
    integer(int64) :: b, e, original_point, n
    logical :: has_exp, redefine

    status = status_type()
    has_exp = .false.
    n = prefix_numeric_value(argument)

    if ((argument%kind == PREFIX_NONE .and. is_use_region()) .or. n == 0) then
        call region_beginning(b, status)
        if (status%ok()) call region_end(e, status)
        if (status%ok()) call buffer_substring(b, e, exp, status)
        if (.not. status%ok()) return
        has_exp = .true.
    else if (n > 0) then
        original_point = point()
        call forward_word(-n)
        e = point()
        call goto_character(original_point)
        call buffer_substring(e, original_point, exp, status)
        if (.not. status%ok()) return
        has_exp = .true.
    end if

    if (has_exp) then
        call read_string(type//" abbrev that expands into """//exp//""": ", name, status)
    else
        call read_string("Undefine "//type//" abbrev: ", name, status)
    end if

    if (.not. status%ok()) return
    redefine = .true.

    if (has_exp) then
        call abbreviation_expansion(name, old, table)

        if (allocated(old)) then
            call read_y_or_n(name//" expands into """//old//"""; redefine? ", redefine, status)
            if (.not. status%ok()) return
        end if
    end if

    if (.not. redefine) return

    if (has_exp) then
        call table%define(downcase_string(name), exp)
    else
        call table%define(downcase_string(name))
    end if
    end procedure add_abbreviation

    module procedure inverse_add_mode_abbreviation
    type(abbreviation_table_type), pointer :: table

    table => mode_table()
    call inverse_add_abbreviation(table, "Mode", n, status)
    end procedure inverse_add_mode_abbreviation

    module procedure inverse_add_global_abbreviation
    call inverse_add_abbreviation(global_abbreviation_table, "Global", n, status)
    end procedure inverse_add_global_abbreviation

    module procedure inverse_add_abbreviation
    character(len=:), allocatable :: name, exp, old
    integer(int64) :: original_point, start, end, moved, position
    type(handle_type) :: saved
    type(status_type) :: st
    logical :: define

    original_point = point()
    call forward_word(1 - argument)
    moved = skip_syntax_backward("^w")
    end = point()
    call forward_word(-1_int64)
    start = point()
    call buffer_substring(start, end, name, status)
    call goto_character(original_point)
    if (.not. status%ok()) return
    call read_string("Expansion for "//type//" abbrev """//name//""": ", exp, status)
    if (.not. status%ok()) return
    define = .true.
    call abbreviation_expansion(name, old, table)

    if (allocated(old)) then
        call read_y_or_n(name//" expands into """//old//"""; redefine? ", define, status)
        if (.not. status%ok()) return
    end if

    if (.not. define) return
    call table%define(downcase_string(name), exp)
    ! (save-excursion (goto-char end) (expand-abbrev))
    call point_marker(saved)
    call goto_character(end)
    call expand_abbreviation(status)
    call marker_position(saved, position, st)
    call goto_character(position)
    call destroy_marker(saved, st)
    end procedure inverse_add_abbreviation

    module procedure abbreviation_prefix_mark
    logical :: no_expand

    no_expand = .false.
    if (present(argument)) no_expand = argument
    status = status_type()
    if (.not. no_expand) call expand_abbreviation(status)
    if (.not. status%ok()) return
    call clear_start_location()
    call point_marker(abbreviation_start_location)
    abbreviation_start_location_buffer = current_buffer()
    call insert("-", status)
    end procedure abbreviation_prefix_mark

    module procedure expand_region_abbreviations
    integer(int64) :: limit, pnt, original_point, position, wbeg
    character(len=:), allocatable :: string, exp
    type(handle_type) :: saved
    type(status_type) :: st
    logical :: query, yes

    query = .true.
    if (present(noquery)) query = .not. noquery
    status = status_type()
    original_point = point()
    call copy_marker_at(original_point, saved, .false., st)
    call goto_character(start)
    limit = point_maximum() - end

    do while (.not. is_end_of_buffer())
        call forward_word(1_int64)
        pnt = point()
        if (pnt > point_maximum() - limit) exit
        position = point()
        call forward_word(-1_int64)
        wbeg = point()
        call goto_character(position)
        call buffer_substring(wbeg, pnt, string, status)
        if (.not. status%ok()) exit
        call abbreviation_expansion(string, exp)

        if (allocated(exp)) then
            deallocate (exp)
            yes = .true.
            if (query) call read_y_or_n(substitute_quotes("Expand `")//string//substitute_quotes("'? "), yes, status)
            if (.not. status%ok()) exit
            if (yes) call expand_abbreviation(status)
            if (.not. status%ok()) exit
        end if
    end do

    call marker_position(saved, position, st)
    call goto_character(position)
    call destroy_marker(saved, st)
    end procedure expand_region_abbreviations

    !> Check if the characters in ABBREVIATION have word syntax in either the
    !> current (if GLOBAL is false) or standard syntax table.  Port of
    !> abbrev.el:abbrev--check-chars.
    subroutine check_characters(abbreviation, global, status)
        character(len=*), intent(in) :: abbreviation
        logical, intent(in) :: global
        type(status_type), intent(out) :: status
        type(handle_type) :: saved_table
        character(len=:), allocatable :: badchars, c
        integer(int64) :: position, at
        type(status_type) :: st

        status = status_type()
        saved_table = syntax_table()
        if (global) call set_syntax_table(standard_syntax_table(), st)

        if (string_match("\W", abbreviation, st) > 0) then
            badchars = ""
            position = 1

            do
                at = string_match("\W", abbreviation, st, start=position)
                if (at == 0) exit
                c = substring_at(abbreviation, at)
                if (index(badchars, c) == 0) badchars = badchars//c
                position = position + 1
            end do

            if (global) then
                call set_error(status, GENERIC_ERROR_CODE, "Some abbrev characters ("//badchars// &
                               ") are not word constituents in the standard syntax")
            else
                call set_error(status, GENERIC_ERROR_CODE, "Some abbrev characters ("//badchars// &
                               ") are not word constituents in this mode")
            end if
        end if

        call set_syntax_table(saved_table, st)
    end subroutine check_characters

    !> The character of the UTF-8 string S at character index I (1-based,
    !> as string_match's positions).  Not a port.
    function substring_at(s, i) result(c)
        character(len=*), intent(in) :: s
        integer(int64), intent(in) :: i
        character(len=:), allocatable :: c
        integer :: k, n, b, start

        n = 0
        start = 0
        c = ""

        do k = 1, len(s)
            b = ichar(s(k:k))

            if (b < 128 .or. b >= 192) then
                n = n + 1
                if (n == i) start = k

                if (n == i + 1) then
                    c = s(start:k - 1)
                    return
                end if
            end if
        end do

        if (start > 0) c = s(start:)
    end function substring_at

    module procedure define_global_abbreviation
    call check_characters(abbreviation, .true., status)
    if (.not. status%ok()) return
    call global_abbreviation_table%define(downcase_string(abbreviation), expansion)
    end procedure define_global_abbreviation

    module procedure define_mode_abbreviation
    type(abbreviation_table_type), pointer :: table

    table => local_abbreviation_table%get_current()

    if (.not. associated(table)) then
        call set_error(status, GENERIC_ERROR_CODE, "Major mode has no abbrev table")
        return
    end if

    call check_characters(abbreviation, .false., status)
    if (.not. status%ok()) return
    call table%define(downcase_string(abbreviation), expansion)
    end procedure define_mode_abbreviation

    ! ------------------------------------------------------------------
    ! Edit-Abbrevs mode and the commands
    ! ------------------------------------------------------------------

    module procedure edit_abbreviations_mode
    logical :: saved

    call major_modes%edit_abbreviations_mode%begin(saved, status)
    if (.not. status%ok()) return
    call major_modes%edit_abbreviations_mode%end(saved, status)
    end procedure edit_abbreviations_mode

    !> Toggle Abbrev mode in the current buffer.
    !>
    !> In Abbrev mode, inserting an abbreviation causes it to expand and
    !> be replaced by its expansion.  Port of abbrev.el:abbrev-mode.
    recursive subroutine execute_abbreviation_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%abbreviation_mode%command(status)
    end subroutine execute_abbreviation_mode

    ! The interactive side of the commands.
    subroutine execute_clear_all_abbreviations(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call clear_all_abbreviations()
    end subroutine execute_clear_all_abbreviations

    recursive subroutine execute_insert_abbreviations(status)
        type(status_type), intent(out) :: status

        call insert_abbreviations(status)
    end subroutine execute_insert_abbreviations

    recursive subroutine execute_list_abbreviations(status)
        type(status_type), intent(out) :: status

        call list_abbreviations(status, local=current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_list_abbreviations

    recursive subroutine execute_edit_abbreviations(status)
        type(status_type), intent(out) :: status

        call edit_abbreviations(status)
    end subroutine execute_edit_abbreviations

    recursive subroutine execute_write_abbreviation_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, expanded

        call expand_file_name(abbreviation_file(), expanded, status)
        if (.not. status%ok()) return
        call read_file_name("Write abbrev file: ", file, status, directory=file_name_directory(expanded), &
                            default_filename=abbreviation_file())
        if (status%ok()) call write_abbreviation_file(status, file)
    end subroutine execute_write_abbreviation_file

    recursive subroutine execute_add_mode_abbreviation(status)
        type(status_type), intent(out) :: status

        call add_mode_abbreviation(current_prefix_argument, status)
    end subroutine execute_add_mode_abbreviation

    recursive subroutine execute_add_global_abbreviation(status)
        type(status_type), intent(out) :: status

        call add_global_abbreviation(current_prefix_argument, status)
    end subroutine execute_add_global_abbreviation

    recursive subroutine execute_inverse_add_mode_abbreviation(status)
        type(status_type), intent(out) :: status

        call inverse_add_mode_abbreviation(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_inverse_add_mode_abbreviation

    recursive subroutine execute_inverse_add_global_abbreviation(status)
        type(status_type), intent(out) :: status

        call inverse_add_global_abbreviation(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_inverse_add_global_abbreviation

    recursive subroutine execute_abbreviation_prefix_mark(status)
        type(status_type), intent(out) :: status

        call abbreviation_prefix_mark(status, argument=current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_abbreviation_prefix_mark

    recursive subroutine execute_expand_region_abbreviations(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call interactive_region(b, e, status)
        if (status%ok()) call expand_region_abbreviations(b, e, status, noquery=current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_expand_region_abbreviations

    recursive subroutine execute_define_global_abbreviation(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: abbreviation, expansion

        call read_string("Define global abbrev: ", abbreviation, status)
        if (status%ok()) call read_string("Expansion for "//abbreviation//": ", expansion, status)
        if (status%ok()) call define_global_abbreviation(abbreviation, expansion, status)
    end subroutine execute_define_global_abbreviation

    recursive subroutine execute_define_mode_abbreviation(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: abbreviation, expansion

        call read_string("Define mode abbrev: ", abbreviation, status)
        if (status%ok()) call read_string("Expansion for "//abbreviation//": ", expansion, status)
        if (status%ok()) call define_mode_abbreviation(abbreviation, expansion, status)
    end subroutine execute_define_mode_abbreviation

    recursive subroutine execute_expand_abbreviation(status)
        type(status_type), intent(out) :: status

        call expand_abbreviation(status)
    end subroutine execute_expand_abbreviation

    recursive subroutine execute_unexpand_abbreviation(status)
        type(status_type), intent(out) :: status

        call unexpand_abbreviation(status)
    end subroutine execute_unexpand_abbreviation

    module procedure initialize_abbreviation
    call ensure_tables()
    call forward_setting(abbreviation_all_caps)
    call minor_modes%abbreviation_mode%define("abbreviation_mode", " Abbrev")
    abbreviation_mode => minor_modes%abbreviation_mode%state
    commands%abbreviation_mode%execute => execute_abbreviation_mode
    call major_modes%edit_abbreviations_mode%define("edit_abbreviations_mode", "Edit-Abbrevs", edit_abbreviations_mode, &
                                                    edit_abbreviations_mode_hook, &
                                                    abbreviation_table=edit_abbreviations_mode_abbreviation_table)
    edit_abbreviations_mode_map = major_modes%edit_abbreviations_mode%map
    commands%clear_all_abbreviations%execute => execute_clear_all_abbreviations
    commands%insert_abbreviations%execute => execute_insert_abbreviations
    commands%list_abbreviations%execute => execute_list_abbreviations
    commands%edit_abbreviations%execute => execute_edit_abbreviations
    commands%write_abbreviation_file%execute => execute_write_abbreviation_file
    commands%add_mode_abbreviation%execute => execute_add_mode_abbreviation
    commands%add_global_abbreviation%execute => execute_add_global_abbreviation
    commands%inverse_add_mode_abbreviation%execute => execute_inverse_add_mode_abbreviation
    commands%inverse_add_global_abbreviation%execute => execute_inverse_add_global_abbreviation
    commands%abbreviation_prefix_mark%execute => execute_abbreviation_prefix_mark
    commands%expand_region_abbreviations%execute => execute_expand_region_abbreviations
    commands%define_global_abbreviation%execute => execute_define_global_abbreviation
    commands%define_mode_abbreviation%execute => execute_define_mode_abbreviation
    commands%expand_abbreviation%execute => execute_expand_abbreviation
    commands%unexpand_abbreviation%execute => execute_unexpand_abbreviation
    call append_locals_clearer(clear_abbreviation_locals)
    call append_save_some_buffers_function(abbreviation_possibly_save)
    end procedure initialize_abbreviation
end submodule abbreviation_commands_mod
