! imenu_mod.f90 -- framework for mode-specific buffer indexes.
! Ported from GNU Emacs 31.1 lisp/imenu.el.
!
! Copyright (C) 1994-1998, 2001-2026 Free Software Foundation, Inc.
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

!> Imenu: a buffer index (the definitions of a buffer and their
!> positions) made by the major mode's `imenu_create_index_function', by
!> default from its `imenu_patterns' or its
!> `imenu_prev_index_position_function' and
!> `imenu_extract_index_name_function', and the command `imenu' (M-g i)
!> that reads an entry of it with completion and goes there.
!>
!> The index is an `imenu_index_type': a flat array of entries, each
!> naming the submenu that holds it (`parent', 0 at the top level), so
!> no derived type contains itself.  A buffer's own values of the
!> buffer-local variables are kept by their setting objects
!> (`call imenu_patterns%set_local(patterns)'); the cached index
!> of a buffer (upstream's imenu--index-alist) is reached through
!> `has_imenu_index', `get_imenu_index' and `set_imenu_index'.  What is
!> not ported yet is listed in the imenu_mod row of PORTING.md.
module imenu_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type, set_error, FM_GENERIC_ERROR, FM_IMENU_UNAVAILABLE
    use handle_mod, only: handle_type
    use print_mod, only: fortran_string
    use data_mod, only: logical_setting_type, string_setting_type, history_setting_type, forward_setting, &
                        append_locals_clearer, number_to_string
    use value_mod, only: value_type, make_list
    use hooks_mod, only: hook_type
    use commands_mod, only: commands
    use buffer_mod, only: buffers, current, buffer_serial_live
    use marker_mod, only: make_marker, set_marker, set_marker_insertion_type, marker_position, destroy_marker
    use editfns_mod, only: point, point_min, point_max, goto_char, widen, narrow_to_region, is_buffer_narrowed, is_bob, &
                           buffer_size
    use cmds_mod, only: beginning_of_line
    use search_mod, only: re_search_backward, match_beginning, match_end, string_match, regexp_quote, case_fold_search, &
                          match_data, set_match_data, SEARCH_NOERROR, SEARCH_MOVE
    use subr_mod, only: match_string_no_properties, find_tag_default
    use syntax_mod, only: parse_state_type, syntax_ppss, syntax_table, set_syntax_table, copy_syntax_table, &
                          destroy_syntax_table, modify_syntax_entry
    use character_mod, only: string_char_and_length
    use font_core_mod, only: font_lock_defaults, font_lock_defaults_type, font_lock_syntax_entry_type
    use expression_motion_mod, only: beginning_of_subprogram
    use xdisp_mod, only: message, invisibility
    use simple_mod, only: push_mark
    use minibuf_table_mod, only: completion_table_type, TABLE_LIST
    use minibuffer_mod, only: completing_read, REQUIRE_MATCH_YES
    implicit none(type, external)
    private

    public :: init_imenu, imenu, cmd_imenu, choose_imenu_buffer_index, imenu_default_create_index_function, &
              imenu_default_goto_function, make_imenu_index, make_imenu_generic_index, flush_imenu_cache, &
              destroy_imenu_markers, truncate_imenu_items, find_imenu_default, find_imenu_entry, flatten_imenu_index, &
              read_imenu_index_item, is_imenu_name_before, is_imenu_position_before, has_imenu_index, get_imenu_index, &
              set_imenu_index, current_imenu_menubar_modified_tick, imenu_beginning_of_subprogram
    public :: imenu_matcher_procedure, imenu_create_index_procedure, imenu_prev_index_position_procedure, &
              imenu_extract_index_name_procedure, imenu_name_lookup_procedure, imenu_goto_procedure

    !> The `position' of the entry that rescans the buffer (upstream's
    !> imenu--rescan-item).
    integer(int64), parameter, public :: IMENU_RESCAN_POSITION = -99
    !> The name of the entry that rescans the buffer (upstream's
    !> imenu--rescan-item).
    character(len=*), parameter, public :: IMENU_RESCAN_NAME = "*Rescan*"

    !> `imenu_max_item_length' when entries are not truncated.
    integer, parameter, public :: IMENU_MAX_ITEM_LENGTH_UNLIMITED = -1

    !> Values of `imenu_flatten': nested indexes (IMENU_FLATTEN_NO), or a
    !> flat list with the section names prepended to the candidates
    !> (IMENU_FLATTEN_PREFIX).
    integer, parameter, public :: IMENU_FLATTEN_NO = 0, IMENU_FLATTEN_PREFIX = 1

    !> One entry of an Imenu index (an element of upstream's index alist):
    !> an item NAME at a buffer position, kept in `marker' when
    !> `imenu_use_markers' was true (else in `position'), or a submenu
    !> (`submenu' true) named NAME whose entries name it as their
    !> `parent' (the index of its entry; 0 for the top level).
    type, public :: imenu_entry_type
        character(len=:), allocatable :: name
        integer(int64) :: position = 0
        type(handle_type) :: marker
        integer :: parent = 0
        logical :: submenu = .false.
    end type imenu_entry_type

    !> A buffer index: its entries, each submenu before the entries it
    !> holds, the entries of one submenu (or of the top level) in order.
    !> Port of imenu.el's index alist (see imenu--index-alist).
    type, public :: imenu_index_type
        type(imenu_entry_type), allocatable :: entries(:)
    contains
        !> Append an entry: NAME at POSITION (or the MARKER), in submenu
        !> PARENT (0 for the top level), a submenu if SUBMENU; K gets its
        !> index.
        procedure :: append => index_append
        !> The buffer position of entry K (its marker's position, if it
        !> has one).
        procedure :: position_of => index_position_of
        !> The indices of the entries of submenu PARENT (0: the top level),
        !> in order.
        procedure :: children => index_children
    end type imenu_index_type

    abstract interface
        !> A function in place of a regexp in `imenu_patterns':
        !> search backwards and FOUND if it finds a definition construct,
        !> setting the match data for that construct.  Not a port.
        subroutine imenu_matcher_procedure(found)
            implicit none(type, external)
            logical, intent(out) :: found
        end subroutine imenu_matcher_procedure

        !> An `imenu_create_index_function': INDEX gets the index of the
        !> current buffer.  Not a port.
        recursive subroutine imenu_create_index_procedure(index, status)
            import :: imenu_index_type, status_type
            implicit none(type, external)
            type(imenu_index_type), intent(out) :: index
            type(status_type), intent(out) :: status
        end subroutine imenu_create_index_procedure

        !> An `imenu_prev_index_position_function': move back to the
        !> previous index position, FOUND if there is one.  Not a port.
        recursive subroutine imenu_prev_index_position_procedure(found)
            implicit none(type, external)
            logical, intent(out) :: found
        end subroutine imenu_prev_index_position_procedure

        !> An `imenu_extract_index_name_function': NAME gets the name of
        !> the index item at point (unallocated for none).  Not a port.
        recursive subroutine imenu_extract_index_name_procedure(name)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: name
        end subroutine imenu_extract_index_name_procedure

        !> An `imenu_name_lookup_function': whether STRING matches the
        !> index item named NAME.  Not a port.
        logical function imenu_name_lookup_procedure(string, name)
            implicit none(type, external)
            character(len=*), intent(in) :: string, name
        end function imenu_name_lookup_procedure

        !> An `imenu_default_goto_function_variable': go to the index item
        !> NAME at POSITION.  Not a port.
        recursive subroutine imenu_goto_procedure(name, position, status)
            import :: int64, status_type
            implicit none(type, external)
            character(len=*), intent(in) :: name
            integer(int64), intent(in) :: position
            type(status_type), intent(out) :: status
        end subroutine imenu_goto_procedure
    end interface

    !> One element of `imenu_patterns': the definitions that
    !> REGEXP matches (or that the procedure MATCHER finds, in place of a
    !> regexp) are put into the submenu MENU_TITLE ("" for the top level
    !> of the index), named by what subexpression SUBEXPRESSION of the
    !> match matched.  Not a port.
    type, public :: imenu_pattern_type
        character(len=:), allocatable :: menu_title
        character(len=:), allocatable :: regexp
        procedure(imenu_matcher_procedure), pointer, nopass :: matcher => null()
        integer :: subexpression = 0
    end type imenu_pattern_type

    ! One buffer's own `imenu_patterns'.
    type :: patterns_entry_type
        integer(int64) :: serial = 0
        type(imenu_pattern_type), allocatable :: patterns(:)
    end type patterns_entry_type

    !> The type of `imenu_patterns': the default `value' and
    !> buffers' own values (`set_local').
    type, public :: imenu_patterns_type
        type(imenu_pattern_type), allocatable :: value(:)
        type(patterns_entry_type), allocatable :: entries(:)
    contains
        !> Give the current buffer its own value MATCHERS (set_local).
        procedure :: set_local => patterns_set_local
        !> The value in the current buffer (empty when unset).
        procedure :: current => current_patterns
        !> Make the current buffer use the default again.
        procedure :: clear_local => patterns_clear_local
    end type imenu_patterns_type

    ! One buffer's own `imenu_syntax_modifications'.
    type :: syntax_entry_type
        integer(int64) :: serial = 0
        type(font_lock_syntax_entry_type), allocatable :: modifications(:)
    end type syntax_entry_type

    !> The type of `imenu_syntax_modifications': the default `value' and
    !> buffers' own values (`set_local').
    type, public :: imenu_syntax_modifications_type
        type(font_lock_syntax_entry_type), allocatable :: value(:)
        type(syntax_entry_type), allocatable :: entries(:)
    contains
        !> Give the current buffer its own value MODIFICATIONS (set_local).
        procedure :: set_local => syntax_set_local
        !> The value in the current buffer (empty when unset).
        procedure :: current => current_syntax
        !> Make the current buffer use the default again.
        procedure :: clear_local => syntax_clear_local
    end type imenu_syntax_modifications_type

    ! One buffer's own `imenu_create_index_function'.
    type :: create_entry_type
        integer(int64) :: serial = 0
        procedure(imenu_create_index_procedure), pointer, nopass :: fn => null()
    end type create_entry_type

    !> The type of `imenu_create_index_function': the default `value'
    !> (assign with `=>') and buffers' own values (`set_local').
    type, public :: imenu_create_index_function_type
        procedure(imenu_create_index_procedure), pointer, nopass :: value => null()
        type(create_entry_type), allocatable :: entries(:)
    contains
        !> Give the current buffer its own value FN (set_local).
        procedure :: set_local => create_set_local
        !> The value in the current buffer.
        procedure :: current => current_create
        !> Make the current buffer use the default again.
        procedure :: clear_local => create_clear_local
    end type imenu_create_index_function_type

    ! One buffer's own `imenu_prev_index_position_function'.
    type :: prev_entry_type
        integer(int64) :: serial = 0
        procedure(imenu_prev_index_position_procedure), pointer, nopass :: fn => null()
    end type prev_entry_type

    !> The type of `imenu_prev_index_position_function': the default
    !> `value' (assign with `=>') and buffers' own values (`set_local';
    !> absent sets the buffer's own value to null).
    type, public :: imenu_prev_index_position_function_type
        procedure(imenu_prev_index_position_procedure), pointer, nopass :: value => null()
        type(prev_entry_type), allocatable :: entries(:)
    contains
        !> Give the current buffer its own value FN (set_local).
        procedure :: set_local => prev_set_local
        !> The value in the current buffer (null when unset).
        procedure :: current => current_prev
        !> Make the current buffer use the default again.
        procedure :: clear_local => prev_clear_local
    end type imenu_prev_index_position_function_type

    ! One buffer's own `imenu_extract_index_name_function'.
    type :: extract_entry_type
        integer(int64) :: serial = 0
        procedure(imenu_extract_index_name_procedure), pointer, nopass :: fn => null()
    end type extract_entry_type

    !> The type of `imenu_extract_index_name_function': the default
    !> `value' (assign with `=>') and buffers' own values (`set_local';
    !> absent sets the buffer's own value to null).
    type, public :: imenu_extract_index_name_function_type
        procedure(imenu_extract_index_name_procedure), pointer, nopass :: value => null()
        type(extract_entry_type), allocatable :: entries(:)
    contains
        !> Give the current buffer its own value FN (set_local).
        procedure :: set_local => extract_set_local
        !> The value in the current buffer (null when unset).
        procedure :: current => current_extract
        !> Make the current buffer use the default again.
        procedure :: clear_local => extract_clear_local
    end type imenu_extract_index_name_function_type

    ! A buffer's cached index (imenu--index-alist: HAS_INDEX false is nil)
    ! and imenu-menubar-modified-tick.
    type :: cache_entry_type
        integer(int64) :: serial = 0
        logical :: has_index = .false.
        type(imenu_index_type) :: index
        integer(int64) :: menubar_modified_tick = 0
    end type cache_entry_type

    type(cache_entry_type), allocatable :: caches(:)

    ! A submenu `make_imenu_generic_index' is gathering (MAIN: the top
    ! level, upstream's title nil), with its items.
    type :: pending_menu_type
        character(len=:), allocatable :: title
        logical :: main = .false.
        type(imenu_entry_type), allocatable :: items(:)
    end type pending_menu_type

    !> True means use markers instead of integers for Imenu buffer positions.
    !>
    !> Setting this to .false. makes Imenu work a little faster but editing the
    !> buffer will make the generated index positions wrong.
    !>
    !> This might not yet be honored by all index-building functions.  Port
    !> of imenu.el imenu-use-markers.
    logical, public :: imenu_use_markers = .true.

    !> If not IMENU_MAX_ITEM_LENGTH_UNLIMITED, truncate Imenu entries to that
    !> length.  Port of imenu.el imenu-max-item-length.
    integer, public :: imenu_max_item_length = 60

    !> True means Imenu should always rescan the buffers.  Port of imenu.el
    !> imenu-auto-rescan.
    logical, public :: imenu_auto_rescan = .false.

    !> Imenu auto-rescan is disabled in buffers larger than this size (in bytes).
    !> Also see `imenu_max_index_time'.  Port of imenu.el
    !> imenu-auto-rescan-maxout.
    integer(int64), public :: imenu_auto_rescan_maxout = 600000

    !> Hooks called after jumping to a place in the buffer.
    !>
    !> Useful things to use here include `reposition_window' and `recenter'.
    !> Port of imenu.el imenu-after-jump-hook.
    type(hook_type), target, public :: imenu_after_jump_hook = hook_type(name="imenu_after_jump_hook")

    !> The replacement string for spaces in index names.
    !> Used when presenting the index in a completion buffer to make the
    !> names work as tokens (unallocated for none).  Port of imenu.el
    !> imenu-space-replacement.
    type(string_setting_type), target, public :: imenu_space_replacement = &
                                                 string_setting_type(name="imenu_space_replacement")

    !> The separator between index names of different levels.
    !> Used for flattening nested indexes with name concatenation.  Port of
    !> imenu.el imenu-level-separator.
    character(len=:), allocatable, public :: imenu_level_separator

    !> Whether to flatten the list of sections in an imenu or show it nested.
    !> If `IMENU_FLATTEN_NO', use nested indexes.
    !> If the value is `IMENU_FLATTEN_PREFIX', pop up the completion buffer
    !> with a flattened menu where section names are prepended to completion
    !> candidates as prefixes.
    !>
    !> The value of `imenu_level_separator', a string, is used to separate
    !> names from different flattened levels, such as section names, from the
    !> names of completion candidates.  Port of imenu.el imenu-flatten (the
    !> values `annotation' and `group' are not ported).
    integer, public :: imenu_flatten = IMENU_FLATTEN_NO

    !> When true, ignore text inside comments and strings.
    !> Only affects `imenu_default_create_index_function' (and any
    !> alternative implementation of `imenu_create_index_function' that
    !> uses `make_imenu_generic_index').  Port of imenu.el
    !> imenu-generic-skip-comments-and-strings.
    logical, public :: imenu_generic_skip_comments_and_strings = .true.

    !> Max time to use when creating imenu indices.  Port of imenu.el
    !> imenu-max-index-time.
    real, public :: imenu_max_index_time = 5.0

    !> List of definition matchers for creating an Imenu index.
    !> Each element of this array is an `imenu_pattern_type':
    !>
    !>   imenu_pattern_type(menu_title=MENU_TITLE, regexp=REGEXP, subexpression=INDEX)
    !>
    !> MENU_TITLE should be "" (in which case the matches for this
    !> element are put in the top level of the buffer index) or a
    !> string (which specifies the title of a submenu into which the
    !> matches are put).
    !> REGEXP is a regular expression matching a definition construct
    !> which is to be displayed in the menu.  In place of REGEXP the
    !> element may give a procedure of no arguments, MATCHER.  It is
    !> expected to search backwards, return FOUND if it finds a
    !> definition construct, and set the match data for that construct.
    !> INDEX is an integer specifying which subexpression of REGEXP
    !> matches the definition's name; this subexpression is displayed as
    !> the menu item.
    !>
    !> The variable `imenu_case_fold_search' determines whether or not
    !> the regexp matches are case sensitive, and `imenu_syntax_modifications'
    !> can be used to alter the syntax table for the search.
    !>
    !> If not empty this pattern is passed to `make_imenu_generic_index' to
    !> create a buffer index.  A major mode gives its buffer its own value
    !> with `imenu_patterns%set_local'.  Port of imenu.el
    !> imenu-generic-expression (FUNCTION and ARGUMENTS of an element are
    !> not ported).
    type(imenu_patterns_type), public :: imenu_patterns

    !> The function to use for creating an index of the current buffer.
    !>
    !> It should be a procedure that gives the index of the current
    !> buffer.  The function is called within a `save_excursion'.
    !>
    !> See `imenu_index_type' for the format of the buffer index.  Port of
    !> imenu.el imenu-create-index-function.
    type(imenu_create_index_function_type), public :: imenu_create_index_function

    !> Function for finding the next index position.
    !>
    !> If `imenu_create_index_function' is set to
    !> `imenu_default_create_index_function', then you must set this variable
    !> to a function that will find the next index, looking backwards in the
    !> file.
    !>
    !> The function should leave point at the place to be connected to the
    !> index and it should give FOUND .false. when it doesn't find another
    !> index.  Port of imenu.el imenu-prev-index-position-function.
    type(imenu_prev_index_position_function_type), public :: imenu_prev_index_position_function

    !> Function for extracting the index item name, given a position.
    !>
    !> This function is called after `imenu_prev_index_position_function'
    !> finds a position for an index item, with point at that position.
    !> It should return the name for that index item.  Port of imenu.el
    !> imenu-extract-index-name-function.
    type(imenu_extract_index_name_function_type), public :: imenu_extract_index_name_function

    !> Function to compare string with index item.
    !>
    !> This function will be called with two strings, and should return
    !> true if they match.
    !>
    !> If null, comparison is done with `=='.
    !> Set this to some other function for more advanced comparisons,
    !> such as "begins with" or "name matches and number of
    !> arguments match".  Port of imenu.el imenu-name-lookup-function (not
    !> buffer-local yet).
    procedure(imenu_name_lookup_procedure), pointer, public :: imenu_name_lookup_function => null()

    !> The default function called when selecting an Imenu item.
    !> The function in this variable is called when selecting a normal
    !> index-item.  Named so because the procedure `imenu_default_goto_function'
    !> has the plain name.  Port of imenu.el imenu-default-goto-function (not
    !> buffer-local yet).
    procedure(imenu_goto_procedure), pointer, public :: imenu_default_goto_function_variable => null()

    !> Defines whether `make_imenu_generic_index' should fold case when matching.
    !>
    !> This variable should be set (only) by initialization code
    !> for modes which use `make_imenu_generic_index'.  If it is not set, but
    !> `font_lock_defaults' is set, then font-lock's setting is used.
    !> Port of imenu.el imenu-case-fold-search.
    type(logical_setting_type), target, public :: imenu_case_fold_search = &
                                                  logical_setting_type(name="imenu_case_fold_search", value=.true.)

    !> Syntax table modifiers to use while in `make_imenu_generic_index'.
    !>
    !> Each entry gives CHARS (one character, or a string whose characters
    !> all get the syntax) and SYNTAX, a syntax description appropriate for
    !> `modify_syntax_entry'.
    !>
    !> This is typically used to give word syntax to characters which
    !> normally have identifier syntax to simplify `imenu_patterns'
    !> and speed-up matching.  Port of imenu.el imenu-syntax-alist.
    type(imenu_syntax_modifications_type), public :: imenu_syntax_modifications

    !> History list for `choose_imenu_buffer_index'.  Port of imenu.el
    !> imenu--history-list.
    type(history_setting_type), target, public :: imenu_history = history_setting_type(name="imenu_history")

contains

    !> Set up Imenu: its settings, the command and the defaults (as
    !> loading imenu.el does).  Not a port.
    subroutine init_imenu()
        call forward_setting(imenu_case_fold_search, local_if_set=.true.)
        call forward_setting(imenu_space_replacement)
        call forward_setting(imenu_history)
        if (.not. allocated(imenu_space_replacement%value)) imenu_space_replacement = "."
        if (.not. allocated(imenu_level_separator)) imenu_level_separator = ":"
        if (.not. allocated(imenu_patterns%value)) allocate (imenu_patterns%value(0))
        if (.not. allocated(imenu_syntax_modifications%value)) allocate (imenu_syntax_modifications%value(0))

        if (.not. associated(imenu_create_index_function%value)) then
            imenu_create_index_function%value => imenu_default_create_index_function
        end if

        if (.not. associated(imenu_prev_index_position_function%value)) then
            imenu_prev_index_position_function%value => imenu_beginning_of_subprogram
        end if

        if (.not. associated(imenu_default_goto_function_variable)) then
            imenu_default_goto_function_variable => imenu_default_goto_function
        end if

        commands%imenu%execute => cmd_imenu
        call append_locals_clearer(clear_imenu_locals)
    end subroutine init_imenu

    !> kill-all-local-variables: the buffer-local variables of imenu.el
    !> kept here (the index is dropped and its markers destroyed).
    !> Not a port.
    subroutine clear_imenu_locals()
        integer :: k

        call imenu_patterns%clear_local()
        call imenu_syntax_modifications%clear_local()
        call imenu_create_index_function%clear_local()
        call imenu_prev_index_position_function%clear_local()
        call imenu_extract_index_name_function%clear_local()
        k = cache_index()
        if (k == 0) return
        call destroy_imenu_markers(caches(k)%index)
        caches(k) = cache_entry_type()
    end subroutine clear_imenu_locals

    !> `beginning_of_subprogram' as the default
    !> `imenu_prev_index_position_function' (upstream's beginning-of-defun
    !> called with no argument): FOUND if it found the beginning of a
    !> subprogram.  Not a port.
    recursive subroutine imenu_beginning_of_subprogram(found)
        logical, intent(out) :: found
        type(status_type) :: st

        call beginning_of_subprogram(1_int64, found, st)
        if (.not. st%ok()) found = .false.
    end subroutine imenu_beginning_of_subprogram

    ! ------------------------------------------------------------------
    ! The index
    ! ------------------------------------------------------------------

    ! Not a port.
    subroutine index_append(self, name, k, position, marker, parent, submenu)
        class(imenu_index_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        integer, intent(out) :: k
        integer(int64), intent(in), optional :: position
        type(handle_type), intent(in), optional :: marker
        integer, intent(in), optional :: parent
        logical, intent(in), optional :: submenu
        type(imenu_entry_type) :: e

        e%name = name
        if (present(position)) e%position = position
        if (present(marker)) e%marker = marker
        if (present(parent)) e%parent = parent
        if (present(submenu)) e%submenu = submenu
        if (.not. allocated(self%entries)) allocate (self%entries(0))
        self%entries = [self%entries, e]
        k = size(self%entries)
    end subroutine index_append

    ! Not a port.
    integer(int64) function index_position_of(self, k) result(pos)
        class(imenu_index_type), intent(in) :: self
        integer, intent(in) :: k
        type(status_type) :: st

        pos = self%entries(k)%position

        if (self%entries(k)%marker%index /= 0) then
            call marker_position(self%entries(k)%marker, pos, st)
            if (.not. st%ok()) pos = self%entries(k)%position
        end if
    end function index_position_of

    ! Not a port.
    function index_children(self, parent) result(list)
        class(imenu_index_type), intent(in) :: self
        integer, intent(in) :: parent
        integer, allocatable :: list(:)
        integer :: k

        allocate (list(0))
        if (.not. allocated(self%entries)) return
        ! (No assignment of an empty array to the allocated list.)
        if (count(self%entries%parent == parent) == 0) return
        list = pack([(k, k=1, size(self%entries))], self%entries%parent == parent)
    end function index_children

    !> Comparison function to sort items depending on their index name.
    !> True if ITEM1's name comes before ITEM2's in the order of their
    !> character codes.  Port of imenu.el:imenu--sort-by-name.
    logical function is_imenu_name_before(item1, item2) result(before)
        type(imenu_entry_type), intent(in) :: item1, item2
        integer :: n

        n = min(len(item1%name), len(item2%name))

        if (item1%name(1:n) /= item2%name(1:n)) then
            before = item1%name(1:n) < item2%name(1:n)
        else
            before = len(item1%name) < len(item2%name)
        end if
    end function is_imenu_name_before

    !> Comparison function to sort items depending on their position.
    !> Return true if and only if ITEM1's position is lower than ITEM2's
    !> position.  Port of imenu.el:imenu--sort-by-position.
    logical function is_imenu_position_before(item1, item2) result(before)
        type(imenu_entry_type), intent(in) :: item1, item2

        before = item1%position < item2%position
    end function is_imenu_position_before

    !> Truncate all names in INDEX to `imenu_max_item_length' characters.
    !> Port of imenu.el:imenu--truncate-items.
    subroutine truncate_imenu_items(index)
        type(imenu_index_type), intent(inout) :: index
        integer :: k

        if (imenu_max_item_length == IMENU_MAX_ITEM_LENGTH_UNLIMITED) return
        if (.not. allocated(index%entries)) return

        do k = 1, size(index%entries)
            index%entries(k)%name = first_characters(index%entries(k)%name, imenu_max_item_length)
        end do
    end subroutine truncate_imenu_items

    ! The first N characters of S (all of S if it is shorter).
    ! Not a port.
    function first_characters(s, n) result(r)
        character(len=*), intent(in) :: s
        integer, intent(in) :: n
        character(len=:), allocatable :: r
        integer :: i, count

        count = 0

        do i = 1, len(s)
            if (iand(ichar(s(i:i), kind=int32), 192_int32) /= 128) then
                if (count == n) then
                    r = s(1:i - 1)
                    return
                end if

                count = count + 1
            end if
        end do

        r = s
    end function first_characters

    !> Destroy the markers of INDEX (they point nowhere afterwards).
    !> Port of imenu.el:imenu--cleanup.
    subroutine destroy_imenu_markers(index)
        type(imenu_index_type), intent(inout) :: index
        type(status_type) :: st
        integer :: k

        if (.not. allocated(index%entries)) return

        do k = 1, size(index%entries)
            if (index%entries(k)%marker%index == 0) cycle
            call destroy_marker(index%entries(k)%marker, st)
            index%entries(k)%marker = handle_type()
        end do
    end subroutine destroy_imenu_markers

    !> Check whether the string NAME is contained in INDEX as a bottom-level
    !> entry (in any submenu); return its index (0 if not).  Port of
    !> imenu.el:imenu--in-alist.
    integer function find_imenu_entry(name, index) result(found)
        character(len=*), intent(in) :: name
        type(imenu_index_type), intent(in) :: index

        found = in_submenu(0)

    contains

        ! The first match among the entries of submenu PARENT and below.
        recursive integer function in_submenu(parent) result(k)
            integer, intent(in) :: parent
            integer, allocatable :: list(:)
            integer :: i
            logical :: matches

            k = 0
            list = index%children(parent)

            do i = 1, size(list)
                if (index%entries(list(i))%submenu) then
                    k = in_submenu(list(i))
                    if (k /= 0) return
                else
                    if (associated(imenu_name_lookup_function)) then
                        matches = imenu_name_lookup_function(name, index%entries(list(i))%name)
                    else
                        matches = name == index%entries(list(i))%name .and. &
                                  len(name) == len(index%entries(list(i))%name)
                    end if

                    if (matches) then
                        k = list(i)
                        return
                    end if
                end if
            end do
        end function in_submenu
    end function find_imenu_entry

    ! ------------------------------------------------------------------
    ! The buffer's cached index (imenu--index-alist)
    ! ------------------------------------------------------------------

    ! The index of the current buffer's cache entry, 0 if none.
    ! Not a port.
    integer function cache_index() result(k)
        k = 0
        if (current == 0 .or. .not. allocated(caches)) return
        k = findloc(caches%serial, buffers(current)%serial, dim=1)
    end function cache_index

    ! Find or make the current buffer's cache entry (reusing the entry of
    ! a closed buffer, whose markers are destroyed), its index K (0
    ! without a current buffer).  Not a port.
    subroutine make_cache_entry(k)
        integer, intent(out) :: k
        type(cache_entry_type) :: entry

        k = cache_index()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(caches)) allocate (caches(0))

        do k = 1, size(caches)
            if (.not. buffer_serial_live(caches(k)%serial)) exit
        end do

        if (k > size(caches)) then
            caches = [caches, entry]
        else
            call destroy_imenu_markers(caches(k)%index)
            caches(k) = entry
        end if

        caches(k)%serial = buffers(current)%serial
    end subroutine make_cache_entry

    !> Whether the current buffer has an index (upstream's imenu--index-alist
    !> has a value).  Not a port.
    logical function has_imenu_index()
        integer :: k

        has_imenu_index = .false.
        k = cache_index()
        if (k > 0) has_imenu_index = caches(k)%has_index
    end function has_imenu_index

    !> INDEX gets the current buffer's index (empty if it has none).
    !> Not a port.
    subroutine get_imenu_index(index)
        type(imenu_index_type), intent(out) :: index
        integer :: k

        allocate (index%entries(0))
        k = cache_index()
        if (k == 0) return
        if (caches(k)%has_index .and. allocated(caches(k)%index%entries)) index = caches(k)%index
    end subroutine get_imenu_index

    !> Make INDEX the current buffer's index (`has_imenu_index' true); the
    !> markers of the index it replaces are destroyed, unless the new index
    !> holds them.  Absent INDEX makes the buffer have none.  Not a port.
    subroutine set_imenu_index(index)
        type(imenu_index_type), intent(in), optional :: index
        integer :: k, i

        call make_cache_entry(k)
        if (k == 0) return

        if (allocated(caches(k)%index%entries) .and. present(index)) then
            if (allocated(index%entries)) then
                do i = 1, size(caches(k)%index%entries)
                    associate (m => caches(k)%index%entries(i)%marker)
                        if (any(index%entries%marker%index == m%index .and. &
                                index%entries%marker%generation == m%generation)) m = handle_type()
                    end associate
                end do
            end if
        end if

        call destroy_imenu_markers(caches(k)%index)
        caches(k)%index = imenu_index_type()
        caches(k)%has_index = present(index)

        if (present(index)) then
            caches(k)%index = index
            if (.not. allocated(caches(k)%index%entries)) allocate (caches(k)%index%entries(0))
        end if
    end subroutine set_imenu_index

    !> Value of `buffer_chars_modified_tick' when the menu bar index was last
    !> updated (0 while it never was).  Port of imenu.el
    !> imenu-menubar-modified-tick.
    integer(int64) function current_imenu_menubar_modified_tick() result(tick)
        integer :: k

        tick = 0
        k = cache_index()
        if (k > 0) tick = caches(k)%menubar_modified_tick
    end function current_imenu_menubar_modified_tick

    !> Create an index for the definitions in the current buffer.
    !> This works by using the hook function `imenu_create_index_function'.
    !> Report an error if the index is empty unless NOERROR is present and
    !> true.  INDEX gets the index, with the entry that rescans the buffer
    !> (IMENU_RESCAN_NAME) first unless `imenu_auto_rescan' is true.
    !>
    !> See `imenu_index_type' for the format of the index.  Port of
    !> imenu.el:imenu--make-index-alist.
    recursive subroutine make_imenu_index(index, status, noerror)
        type(imenu_index_type), intent(out) :: index
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noerror
        type(imenu_index_type) :: made, built
        procedure(imenu_create_index_procedure), pointer :: create
        integer(int64) :: saved_point, saved_beginning, saved_end
        logical :: narrowed, quiet
        integer :: k, i

        status = status_type()
        quiet = .false.
        if (present(noerror)) quiet = noerror

        if (.not. has_imenu_index() .or. (imenu_auto_rescan .and. buffer_size() <= imenu_auto_rescan_maxout)) then
            ! Get the index; truncate if necessary.
            saved_point = point()
            saved_beginning = point_min()
            saved_end = point_max()
            narrowed = is_buffer_narrowed()
            call widen()
            create => imenu_create_index_function%current()
            call create(made, status)
            if (narrowed) call narrow_to_region(saved_beginning, saved_end, status)
            call goto_char(saved_point)
            if (.not. status%ok()) return
            call truncate_imenu_items(made)

            if (allocated(made%entries)) then
                if (size(made%entries) > 0) then
                    call set_imenu_index(made)
                else
                    call set_imenu_index()
                end if
            else
                call set_imenu_index()
            end if
        end if

        if (.not. has_imenu_index() .and. .not. quiet) then
            call set_error(status, FM_IMENU_UNAVAILABLE, &
                           fortran_string("No items suitable for an index found in this buffer"))
            return
        end if

        if (.not. has_imenu_index()) then
            call make_cache_entry(k)
            if (k > 0) caches(k)%has_index = .true.
        end if

        call get_imenu_index(built)

        if (imenu_auto_rescan) then
            index = built
        else
            ! Add a rescan option to the index.
            call index%append(IMENU_RESCAN_NAME, k, position=IMENU_RESCAN_POSITION)

            do i = 1, size(built%entries)
                if (built%entries(i)%parent /= 0) built%entries(i)%parent = built%entries(i)%parent + 1
            end do

            index%entries = [index%entries, built%entries]
        end if
    end subroutine make_imenu_index

    !> Flush the current imenu cache.
    !> This forces a full rescan of the buffer to recreate the index
    !> next time `imenu' is invoked.  Port of imenu.el:imenu-flush-cache.
    subroutine flush_imenu_cache()
        call set_imenu_index()
        call clear_cache_flag()

    contains

        ! (setq imenu--index-alist nil).
        subroutine clear_cache_flag()
            integer :: k

            k = cache_index()
            if (k > 0) caches(k)%has_index = .false.
        end subroutine clear_cache_flag
    end subroutine flush_imenu_cache

    ! ------------------------------------------------------------------
    ! Making the index
    ! ------------------------------------------------------------------

    !> Default function to create an index of the current buffer.
    !>
    !> The most general method is to move point to end of buffer, then
    !> repeatedly call `imenu_prev_index_position_function' and
    !> `imenu_extract_index_name_function'.  All the results returned by the
    !> latter are gathered into an index.  This method is used if those two
    !> variables are set.
    !>
    !> The alternate method, which is the one most often used, is to call
    !> `make_imenu_generic_index' with `imenu_patterns' as argument.
    !> Port of imenu.el:imenu-default-create-index-function.
    recursive subroutine imenu_default_create_index_function(index, status)
        type(imenu_index_type), intent(out) :: index
        type(status_type), intent(out) :: status
        procedure(imenu_prev_index_position_procedure), pointer :: prev
        procedure(imenu_extract_index_name_procedure), pointer :: extract
        type(imenu_pattern_type), allocatable :: patterns(:)
        type(imenu_entry_type), allocatable :: found_items(:)
        type(imenu_entry_type) :: item
        character(len=:), allocatable :: name
        integer(int64) :: pos, here, start_count, now, rate
        integer :: k, appended
        logical :: found

        status = status_type()
        allocate (index%entries(0))
        prev => imenu_prev_index_position_function%current()
        extract => imenu_extract_index_name_function%current()

        if (associated(prev) .and. associated(extract)) then
            allocate (found_items(0))
            pos = point_max()
            call system_clock(start_count, rate)
            call goto_char(pos)

            ! Search for the function.
            do
                call prev(found)
                if (.not. found) exit
                ! Don't use an excessive amount of time.
                call system_clock(now)
                if (real(now - start_count)/real(max(1_int64, rate)) >= imenu_max_index_time) exit

                if (.not. point() < pos) then
                    call set_error(status, FM_GENERIC_ERROR, "Infinite loop at "//buffers(current)%name//":"// &
                                   number_to_string(pos)// &
                                   ": imenu_prev_index_position_function does not move point")
                    return
                end if

                pos = point()
                here = point()
                call extract(name)
                call goto_char(here)

                if (allocated(name)) then
                    item = imenu_entry_type()
                    item%name = name
                    item%position = point()
                    if (imenu_use_markers) call make_position_marker(point(), item%marker)
                    found_items = [item, found_items]
                end if
            end do

            do k = 1, size(found_items)
                call index%append(found_items(k)%name, appended, position=found_items(k)%position, &
                                  marker=found_items(k)%marker)
            end do
        else
            ! Use generic expression if possible.
            patterns = imenu_patterns%current()

            if (size(patterns) > 0) then
                call make_imenu_generic_index(patterns, index, status)
            else
                call set_error(status, FM_IMENU_UNAVAILABLE, &
                               fortran_string("This buffer cannot use `imenu_default_create_index_function'"))
            end if
        end if
    end subroutine imenu_default_create_index_function

    ! A marker at POS of the current buffer that advances on insertion
    ! there (copy-marker POS t).  Not a port.
    subroutine make_position_marker(pos, m)
        integer(int64), intent(in) :: pos
        type(handle_type), intent(out) :: m
        type(status_type) :: st

        call make_marker(m)
        call set_marker(m, pos, st)
        call set_marker_insertion_type(m, .true., st)
    end subroutine make_position_marker

    !> Return an index of the current buffer based on PATTERNS.
    !> PATTERNS should be an array of the same form as
    !> `imenu_patterns'.
    !>
    !> If `imenu_generic_skip_comments_and_strings' is true, this ignores
    !> text inside comments and strings.
    !>
    !> If `imenu_case_fold_search' is true, this ignores case.
    !>
    !> The entries of INDEX are items named by the matched subexpression at
    !> the beginning of the line where it starts, in submenus named by the
    !> patterns' menu titles.  Port of imenu.el:imenu--generic-function.
    recursive subroutine make_imenu_generic_index(patterns, index, status)
        type(imenu_pattern_type), intent(in) :: patterns(:)
        type(imenu_index_type), intent(out) :: index
        type(status_type), intent(out) :: status
        type(pending_menu_type), allocatable :: menus(:)
        type(pending_menu_type) :: new_menu
        type(imenu_entry_type) :: item
        type(font_lock_syntax_entry_type), allocatable :: modifications(:)
        type(font_lock_defaults_type) :: defaults
        type(handle_type) :: old_table, table
        type(parse_state_type) :: ppss
        type(status_type) :: st
        integer(int64), allocatable :: saved_match(:)
        integer(int64) :: start, beginning, at, start_count, now, rate
        integer :: p, m, k, j
        logical :: saved_fold, fold, found, invisible, duplicate, matched

        status = status_type()
        allocate (index%entries(0))
        allocate (menus(0))

        if (imenu_case_fold_search%local_p() .or. .not. font_lock_defaults%present_p()) then
            fold = imenu_case_fold_search%value
        else
            defaults = font_lock_defaults%current()
            fold = defaults%case_fold
        end if

        call system_clock(start_count, rate)
        old_table = syntax_table()
        call copy_syntax_table(table, status, old_table)
        if (.not. status%ok()) return
        ! Modify the syntax table used while matching regexps.
        modifications = imenu_syntax_modifications%current()

        do k = 1, size(modifications)
            call modify_characters(modifications(k))
        end do

        call goto_char(point_max())
        saved_match = match_data()
        saved_fold = case_fold_search%value
        case_fold_search = fold
        call set_syntax_table(table, st)

        ! Map over the elements of imenu-generic-expression
        ! (typically functions, variables ...).
        patterns_loop: do p = 1, size(patterns)
            ! Go backwards for convenience of adding items in order.
            call goto_char(point_max())

            do
                if (associated(patterns(p)%matcher)) then
                    call patterns(p)%matcher(found)
                else
                    call re_search_backward(patterns(p)%regexp, at, status, noerror=SEARCH_NOERROR)
                    if (.not. status%ok()) exit patterns_loop
                    found = at /= 0

                    if (found) then
                        ! Do not count invisible definitions.
                        invisible = invisibility(point()) /= 0

                        do while (invisible .and. .not. is_bob())
                            call re_search_backward(patterns(p)%regexp, at, status, noerror=SEARCH_MOVE)
                            if (.not. status%ok()) exit patterns_loop
                            invisible = at == 0
                        end do

                        found = .not. invisible
                    end if
                end if

                if (.not. found) exit
                ! Exit the loop if we get an empty match,
                ! because it means a bad regexp was specified.
                at = match_beginning(0, status)
                if (.not. status%ok()) exit patterns_loop
                if (at == match_end(0, status)) exit
                if (.not. status%ok()) exit patterns_loop
                ! Don't take an excessive amount of time.
                call system_clock(now)

                if (real(now - start_count)/real(max(1_int64, rate)) >= imenu_max_index_time) then
                    call message("`imenu_max_index_time' exceeded")
                    exit
                end if

                start = point()
                ! Record the start of the line in which the match starts.
                ! That's the official position of this definition.
                at = match_beginning(patterns(p)%subexpression, status)
                if (.not. status%ok()) exit patterns_loop
                call goto_char(at)
                call beginning_of_line()
                beginning = point()
                ! Add this sort of submenu only when we've found an
                ! item for it, avoiding empty, duff menus.
                m = menu_index(patterns(p)%menu_title)

                if (m == 0) then
                    new_menu = pending_menu_type()
                    new_menu%title = patterns(p)%menu_title
                    new_menu%main = len(patterns(p)%menu_title) == 0
                    allocate (new_menu%items(0))
                    menus = [new_menu, menus]
                    m = 1
                end if

                item = imenu_entry_type()
                item%name = match_string_no_properties(patterns(p)%subexpression, matched=matched)
                if (.not. matched) item%name = ""
                item%position = beginning
                ! Insert the item unless it is already present.
                duplicate = .false.

                do j = 1, size(menus(m)%items)
                    if (menus(m)%items(j)%position == beginning .and. menus(m)%items(j)%name == item%name .and. &
                        len(menus(m)%items(j)%name) == len(item%name)) duplicate = .true.
                end do

                if (.not. duplicate .and. imenu_generic_skip_comments_and_strings) then
                    call goto_char(start)
                    call syntax_ppss(ppss)
                    duplicate = ppss%comstr_start /= 0
                end if

                if (.not. duplicate) menus(m)%items = [item, menus(m)%items]
                ! Go to the start of the match, to make sure we
                ! keep making progress backwards.
                call goto_char(start)
            end do
        end do patterns_loop

        call set_syntax_table(old_table, st)
        call destroy_syntax_table(table, st)
        case_fold_search = saved_fold
        call set_match_data(saved_match)
        if (.not. status%ok()) return

        ! Sort each submenu by position.
        ! This is in case one submenu gets items from two different regexps.
        do m = 1, size(menus)
            call sort_by_position(menus(m)%items)
        end do

        ! Remove any empty menus (that can happen because of skipping
        ! things inside comments or strings); the submenus come first,
        ! then the items of the top level.
        do m = 1, size(menus)
            if (menus(m)%main .or. size(menus(m)%items) == 0) cycle
            call index%append(menus(m)%title, k, submenu=.true.)
            call append_items(menus(m)%items, k)
        end do

        do m = 1, size(menus)
            if (menus(m)%main) call append_items(menus(m)%items, 0)
        end do

    contains

        ! The index in MENUS of the menu titled TITLE ("" the top level),
        ! 0 if none.
        integer function menu_index(title) result(i)
            character(len=*), intent(in) :: title

            do i = 1, size(menus)
                if (len(title) == 0) then
                    if (menus(i)%main) return
                else if (.not. menus(i)%main) then
                    if (menus(i)%title == title .and. len(menus(i)%title) == len(title)) return
                end if
            end do

            i = 0
        end function menu_index

        ! Give the characters of MODIFICATION its syntax in TABLE.
        subroutine modify_characters(modification)
            type(font_lock_syntax_entry_type), intent(in) :: modification
            integer(int64) :: pos
            integer(int32) :: c, length
            type(status_type) :: st

            pos = 1

            do while (pos <= len(modification%chars, kind=int64))
                call string_char_and_length(modification%chars, pos, c, length)
                call modify_syntax_entry(int(c), int(c), modification%syntax, st, table)
                pos = pos + max(1_int32, length)
            end do
        end subroutine modify_characters

        ! Append ITEMS to INDEX in submenu PARENT, with markers if
        ! `imenu_use_markers'.
        subroutine append_items(items, parent)
            type(imenu_entry_type), intent(in) :: items(:)
            integer, intent(in) :: parent
            type(handle_type) :: marker
            integer :: i, k

            do i = 1, size(items)
                marker = handle_type()
                if (imenu_use_markers) call make_position_marker(items(i)%position, marker)
                call index%append(items(i)%name, k, position=items(i)%position, marker=marker, parent=parent)
            end do
        end subroutine append_items
    end subroutine make_imenu_generic_index

    ! Sort ITEMS by position, keeping the order of equal ones (a stable
    ! insertion sort, as `sort' with imenu--sort-by-position).
    ! Not a port.
    subroutine sort_by_position(items)
        type(imenu_entry_type), intent(inout) :: items(:)
        type(imenu_entry_type) :: x
        integer :: i, j

        do i = 2, size(items)
            x = items(i)
            j = i - 1

            do while (j >= 1)
                if (.not. is_imenu_position_before(x, items(j))) exit
                items(j + 1) = items(j)
                j = j - 1
            end do

            items(j + 1) = x
        end do
    end subroutine sort_by_position

    ! ------------------------------------------------------------------
    ! Choosing an entry
    ! ------------------------------------------------------------------

    !> Fuzzily find an item based on GUESS among the NAMES of the top level
    !> of INDEX; "" if none.  Port of imenu.el:imenu-find-default.
    function find_imenu_default(guess, index) result(found)
        character(len=*), intent(in) :: guess
        type(imenu_index_type), intent(in) :: index
        character(len=:), allocatable :: found
        character(len=:), allocatable :: regexp
        integer, allocatable :: list(:)
        type(status_type) :: st
        logical :: saved_fold
        integer :: r, i

        found = ""
        list = index%children(0)

        do i = 1, size(list)
            if (index%entries(list(i))%name == guess .and. len(index%entries(list(i))%name) == len(guess)) then
                found = guess
                return
            end if
        end do

        saved_fold = case_fold_search%value
        case_fold_search = .true.

        search: do r = 1, 4
            select case (r)
            case (1)
                regexp = "\`"//regexp_quote(guess)//"\'"
            case (2)
                regexp = "\`"//regexp_quote(guess)
            case (3)
                regexp = regexp_quote(guess)//"\'"
            case default
                regexp = regexp_quote(guess)
            end select

            do i = 1, size(list)
                if (string_match(regexp, index%entries(list(i))%name, st) > 0) then
                    found = index%entries(list(i))%name
                    exit search
                end if
            end do
        end do search

        case_fold_search = saved_fold
    end function find_imenu_default

    !> Let the user select from the entries of submenu PARENT of INDEX (0:
    !> the top level) in a completion buffer with PROMPT.
    !>
    !> CHOSEN gets the index of the entry, a submenu's entry chosen in
    !> turn from its own (0 if none).  Port of imenu.el:imenu--completion-buffer.
    recursive subroutine read_imenu_index_item(index, parent, chosen, status, prompt)
        type(imenu_index_type), intent(in) :: index
        integer, intent(in) :: parent
        integer, intent(out) :: chosen
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prompt
        type(imenu_index_type) :: prepared
        type(completion_table_type) :: table
        type(value_type), allocatable :: names(:)
        character(len=:), allocatable :: name, full_prompt, default, result
        integer, allocatable :: list(:)
        logical :: found
        integer :: i, k

        chosen = 0
        status = status_type()
        ! Create a list for this buffer only when needed.
        prepared = index
        list = index%children(parent)

        if (allocated(imenu_space_replacement%value)) then
            do i = 1, size(list)
                prepared%entries(list(i))%name = replace_spaces(index%entries(list(i))%name)
            end do
        end if

        call find_tag_default(name, found)

        if (found) then
            default = find_imenu_default(name, submenu_index(prepared, parent))
            if (len(default) > 0) name = default
        end if

        if (present(prompt)) then
            full_prompt = prompt
        else
            default = ""

            if (found) then
                ! Default to `name' if it's in the index.
                if (find_imenu_entry(name, submenu_index(prepared, parent)) /= 0) default = name
            end if

            full_prompt = "Index item"
            if (len(default) > 0) full_prompt = full_prompt//" (default "//default//")"
            full_prompt = full_prompt//": "
        end if

        allocate (names(size(list)))

        do i = 1, size(list)
            names(i) = value_type(text=prepared%entries(list(i))%name)
        end do

        table%kind = TABLE_LIST
        table%list = make_list(names)
        table%category = "imenu"

        if (found) then
            call completing_read(full_prompt, table, result, status, require_match=REQUIRE_MATCH_YES, &
                                 history="imenu_history", default=name)
        else
            call completing_read(full_prompt, table, result, status, require_match=REQUIRE_MATCH_YES, &
                                 history="imenu_history")
        end if

        if (.not. status%ok()) return

        do i = 1, size(list)
            k = list(i)

            if (prepared%entries(k)%name == result .and. len(prepared%entries(k)%name) == len(result)) then
                if (index%entries(k)%submenu) then
                    call read_imenu_index_item(index, k, chosen, status, full_prompt)
                else
                    chosen = k
                end if

                return
            end if
        end do

    contains

        ! NAME with its spaces replaced by the first character of
        ! `imenu_space_replacement' (subst-char-in-string).
        function replace_spaces(name) result(r)
            character(len=*), intent(in) :: name
            character(len=:), allocatable :: r
            integer :: j

            r = name
            if (len(imenu_space_replacement%value) == 0) return

            do j = 1, len(r)
                if (r(j:j) == " ") r(j:j) = imenu_space_replacement%value(1:1)
            end do
        end function replace_spaces
    end subroutine read_imenu_index_item

    ! INDEX with the entries of submenu PARENT at its top level (a submenu
    ! taken as an index of its own).  Not a port.
    function submenu_index(index, parent) result(sub)
        type(imenu_index_type), intent(in) :: index
        integer, intent(in) :: parent
        type(imenu_index_type) :: sub
        integer :: k

        sub = index
        if (parent == 0) return

        do k = 1, size(sub%entries)
            if (sub%entries(k)%parent == parent) then
                sub%entries(k)%parent = 0
            else if (sub%entries(k)%parent == 0) then
                sub%entries(k)%parent = -1
            end if
        end do
    end function submenu_index

    !> Return a flat index made from the nested INDEX.
    !> If CONCAT_NAMES is true, a nested entry has the names of its
    !> submenus and `imenu_level_separator' prepended to its name.
    !> Port of imenu.el:imenu--flatten-index-alist.
    function flatten_imenu_index(index, concat_names) result(flat)
        type(imenu_index_type), intent(in) :: index
        logical, intent(in) :: concat_names
        type(imenu_index_type) :: flat

        allocate (flat%entries(0))
        call flatten(0, "", .false.)

    contains

        ! The entries below PARENT, whose names have PREFIX (HAS_PREFIX).
        recursive subroutine flatten(parent, prefix, has_prefix)
            integer, intent(in) :: parent
            character(len=*), intent(in) :: prefix
            logical, intent(in) :: has_prefix
            character(len=:), allocatable :: new_prefix
            integer, allocatable :: list(:)
            integer :: i, k

            list = index%children(parent)

            do i = 1, size(list)
                associate (e => index%entries(list(i)))
                    new_prefix = e%name
                    if (has_prefix) new_prefix = prefix//imenu_level_separator//e%name

                    if (.not. e%submenu) then
                        if (concat_names) then
                            call flat%append(new_prefix, k, position=e%position, marker=e%marker)
                        else
                            call flat%append(e%name, k, position=e%position, marker=e%marker)
                        end if
                    else
                        call flatten(list(i), new_prefix, concat_names)
                    end if
                end associate
            end do
        end subroutine flatten
    end function flatten_imenu_index

    !> Let the user select from a buffer index and return the chosen index
    !> entry ITEM (FOUND .false. if none).
    !>
    !> A completion buffer is used and the user is prompted with PROMPT.
    !>
    !> If you call this function with INDEX, then it lets the user select
    !> from INDEX.
    !>
    !> With no INDEX, it calls `make_imenu_index' to create the index.
    !> Port of imenu.el:imenu-choose-buffer-index (the mouse menu is not
    !> ported).
    recursive subroutine choose_imenu_buffer_index(item, found, status, prompt, index)
        type(imenu_entry_type), intent(out) :: item
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prompt
        type(imenu_index_type), intent(in), optional :: index
        type(imenu_index_type) :: alist
        integer :: chosen

        found = .false.

        ! Create a list for this buffer only when needed.
        do
            if (present(index)) then
                alist = index
                status = status_type()
            else
                call make_imenu_index(alist, status)
                if (.not. status%ok()) return
            end if

            if (imenu_flatten /= IMENU_FLATTEN_NO) alist = flatten_imenu_index(alist, .true.)

            if (present(prompt)) then
                call read_imenu_index_item(alist, 0, chosen, status, prompt)
            else
                call read_imenu_index_item(alist, 0, chosen, status)
            end if

            if (.not. status%ok()) return
            if (chosen == 0) return
            item = alist%entries(chosen)
            item%position = alist%position_of(chosen)
            found = .true.
            if (.not. (item%name == IMENU_RESCAN_NAME .and. item%position == IMENU_RESCAN_POSITION)) return
            call flush_imenu_cache()
            found = .false.
        end do
    end subroutine choose_imenu_buffer_index

    !> Move to the given position.
    !>
    !> NAME is ignored.  POSITION is where to move.  The ignored argument
    !> just makes this function have the same interface as a function
    !> placed in a special index item.  Port of
    !> imenu.el:imenu-default-goto-function.
    recursive subroutine imenu_default_goto_function(name, position, status)
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: position
        type(status_type), intent(out) :: status

        status = status_type()
        if (len(name) > 0) continue
        ! Widen if outside narrowing.
        if (position < point_min() .or. position > point_max()) call widen()
        call goto_char(position)
    end subroutine imenu_default_goto_function

    !> Jump to a place in the buffer chosen using a buffer menu.
    !> The index entry ITEM (or the top-level entry named NAME) specifies
    !> the position.  See `choose_imenu_buffer_index' for more information.
    !> Port of imenu.el:imenu.
    recursive subroutine imenu(status, name, item)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: name
        type(imenu_entry_type), intent(in), optional :: item
        type(imenu_index_type) :: index
        type(imenu_entry_type) :: chosen
        integer, allocatable :: list(:)
        integer :: i

        status = status_type()

        if (present(item)) then
            chosen = item
        else if (present(name)) then
            ! Convert a string to an index entry.
            call make_imenu_index(index, status)
            if (.not. status%ok()) return
            list = index%children(0)

            do i = 1, size(list)
                if (index%entries(list(i))%name == name .and. len(index%entries(list(i))%name) == len(name)) exit
            end do

            if (i > size(list)) return
            chosen = index%entries(list(i))
            chosen%position = index%position_of(list(i))
        else
            return
        end if

        if (chosen%submenu) then
            call set_error(status, FM_GENERIC_ERROR, "Unknown imenu item: "//fortran_string(chosen%name))
            return
        end if

        call push_mark(nomsg=.true.)
        call imenu_default_goto_function_variable(chosen%name, chosen%position, status)
        if (.not. status%ok()) return
        call imenu_after_jump_hook%execute(status)
    end subroutine imenu

    !> Jump to a place in the buffer chosen using a buffer menu or mouse menu.
    !> INDEX-ITEM specifies the position.  See `choose_imenu_buffer_index'
    !> for more information.  Port of imenu.el:imenu.
    recursive subroutine cmd_imenu(status)
        type(status_type), intent(out) :: status
        type(imenu_entry_type) :: item
        logical :: found

        call choose_imenu_buffer_index(item, found, status)
        if (.not. status%ok()) return
        if (found) call imenu(status, item=item)
    end subroutine cmd_imenu

    ! ------------------------------------------------------------------
    ! Buffer-local values
    ! ------------------------------------------------------------------

    ! The serial number of the current buffer (-1 without one, which no
    ! entry has).  Not a port.
    integer(int64) function current_serial() result(serial)
        serial = -1
        if (current > 0) serial = buffers(current)%serial
    end function current_serial

    ! The index of a free entry in SERIALS (one of a closed buffer), 0 if
    ! none.  Not a port.
    integer function free_index(serials) result(i)
        integer(int64), intent(in) :: serials(:)

        do i = 1, size(serials)
            if (.not. buffer_serial_live(serials(i))) return
        end do

        i = 0
    end function free_index

    ! Not a port.
    subroutine patterns_set_local(self, patterns)
        class(imenu_patterns_type), intent(inout) :: self
        type(imenu_pattern_type), intent(in) :: patterns(:)
        type(patterns_entry_type) :: entry
        integer :: i

        if (current == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))
        i = findloc(self%entries%serial, current_serial(), dim=1)
        if (i == 0) i = free_index([self%entries%serial])

        if (i == 0) then
            self%entries = [self%entries, entry]
            i = size(self%entries)
        end if

        self%entries(i)%serial = buffers(current)%serial
        if (allocated(self%entries(i)%patterns)) deallocate (self%entries(i)%patterns)
        allocate (self%entries(i)%patterns(size(patterns)))
        self%entries(i)%patterns(:) = patterns(:)
    end subroutine patterns_set_local

    ! Not a port.
    function current_patterns(self) result(patterns)
        class(imenu_patterns_type), intent(in) :: self
        type(imenu_pattern_type), allocatable :: patterns(:)
        integer :: i

        i = 0
        if (allocated(self%entries)) i = findloc(self%entries%serial, current_serial(), dim=1)

        if (i > 0) then
            allocate (patterns(size(self%entries(i)%patterns)))
            patterns(:) = self%entries(i)%patterns(:)
        else if (allocated(self%value)) then
            allocate (patterns(size(self%value)))
            patterns(:) = self%value(:)
        else
            allocate (patterns(0))
        end if
    end function current_patterns

    ! Not a port.
    subroutine patterns_clear_local(self)
        class(imenu_patterns_type), intent(inout) :: self
        integer :: i

        if (.not. allocated(self%entries)) return
        i = findloc(self%entries%serial, current_serial(), dim=1)
        if (i == 0) return
        self%entries(i)%serial = 0
        if (allocated(self%entries(i)%patterns)) deallocate (self%entries(i)%patterns)
    end subroutine patterns_clear_local

    ! Not a port.
    subroutine syntax_set_local(self, modifications)
        class(imenu_syntax_modifications_type), intent(inout) :: self
        type(font_lock_syntax_entry_type), intent(in) :: modifications(:)
        type(syntax_entry_type) :: entry
        integer :: i

        if (current == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))
        i = findloc(self%entries%serial, current_serial(), dim=1)
        if (i == 0) i = free_index([self%entries%serial])

        if (i == 0) then
            self%entries = [self%entries, entry]
            i = size(self%entries)
        end if

        self%entries(i)%serial = buffers(current)%serial
        if (allocated(self%entries(i)%modifications)) deallocate (self%entries(i)%modifications)
        allocate (self%entries(i)%modifications(size(modifications)))
        self%entries(i)%modifications(:) = modifications(:)
    end subroutine syntax_set_local

    ! Not a port.
    function current_syntax(self) result(modifications)
        class(imenu_syntax_modifications_type), intent(in) :: self
        type(font_lock_syntax_entry_type), allocatable :: modifications(:)
        integer :: i

        i = 0
        if (allocated(self%entries)) i = findloc(self%entries%serial, current_serial(), dim=1)

        if (i > 0) then
            allocate (modifications(size(self%entries(i)%modifications)))
            modifications(:) = self%entries(i)%modifications(:)
        else if (allocated(self%value)) then
            allocate (modifications(size(self%value)))
            modifications(:) = self%value(:)
        else
            allocate (modifications(0))
        end if
    end function current_syntax

    ! Not a port.
    subroutine syntax_clear_local(self)
        class(imenu_syntax_modifications_type), intent(inout) :: self
        integer :: i

        if (.not. allocated(self%entries)) return
        i = findloc(self%entries%serial, current_serial(), dim=1)
        if (i == 0) return
        self%entries(i)%serial = 0
        if (allocated(self%entries(i)%modifications)) deallocate (self%entries(i)%modifications)
    end subroutine syntax_clear_local

    ! Not a port.
    subroutine create_set_local(self, fn)
        class(imenu_create_index_function_type), intent(inout) :: self
        procedure(imenu_create_index_procedure) :: fn
        type(create_entry_type) :: entry
        integer :: i

        if (current == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))
        i = findloc(self%entries%serial, current_serial(), dim=1)
        if (i == 0) i = free_index([self%entries%serial])

        if (i == 0) then
            self%entries = [self%entries, entry]
            i = size(self%entries)
        end if

        self%entries(i)%serial = buffers(current)%serial
        self%entries(i)%fn => fn
    end subroutine create_set_local

    ! Not a port.
    function current_create(self) result(fn)
        class(imenu_create_index_function_type), intent(in) :: self
        procedure(imenu_create_index_procedure), pointer :: fn
        integer :: i

        fn => self%value
        if (.not. allocated(self%entries)) return
        i = findloc(self%entries%serial, current_serial(), dim=1)
        if (i > 0) fn => self%entries(i)%fn
    end function current_create

    ! Not a port.
    subroutine create_clear_local(self)
        class(imenu_create_index_function_type), intent(inout) :: self
        integer :: i

        if (.not. allocated(self%entries)) return
        i = findloc(self%entries%serial, current_serial(), dim=1)
        if (i == 0) return
        self%entries(i)%serial = 0
        self%entries(i)%fn => null()
    end subroutine create_clear_local

    ! Not a port.
    subroutine prev_set_local(self, fn)
        class(imenu_prev_index_position_function_type), intent(inout) :: self
        procedure(imenu_prev_index_position_procedure), optional :: fn
        type(prev_entry_type) :: entry
        integer :: i

        if (current == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))
        i = findloc(self%entries%serial, current_serial(), dim=1)
        if (i == 0) i = free_index([self%entries%serial])

        if (i == 0) then
            self%entries = [self%entries, entry]
            i = size(self%entries)
        end if

        self%entries(i)%serial = buffers(current)%serial
        self%entries(i)%fn => null()
        if (present(fn)) self%entries(i)%fn => fn
    end subroutine prev_set_local

    ! Not a port.
    function current_prev(self) result(fn)
        class(imenu_prev_index_position_function_type), intent(in) :: self
        procedure(imenu_prev_index_position_procedure), pointer :: fn
        integer :: i

        fn => self%value
        if (.not. allocated(self%entries)) return
        i = findloc(self%entries%serial, current_serial(), dim=1)
        if (i > 0) fn => self%entries(i)%fn
    end function current_prev

    ! Not a port.
    subroutine prev_clear_local(self)
        class(imenu_prev_index_position_function_type), intent(inout) :: self
        integer :: i

        if (.not. allocated(self%entries)) return
        i = findloc(self%entries%serial, current_serial(), dim=1)
        if (i == 0) return
        self%entries(i)%serial = 0
        self%entries(i)%fn => null()
    end subroutine prev_clear_local

    ! Not a port.
    subroutine extract_set_local(self, fn)
        class(imenu_extract_index_name_function_type), intent(inout) :: self
        procedure(imenu_extract_index_name_procedure), optional :: fn
        type(extract_entry_type) :: entry
        integer :: i

        if (current == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))
        i = findloc(self%entries%serial, current_serial(), dim=1)
        if (i == 0) i = free_index([self%entries%serial])

        if (i == 0) then
            self%entries = [self%entries, entry]
            i = size(self%entries)
        end if

        self%entries(i)%serial = buffers(current)%serial
        self%entries(i)%fn => null()
        if (present(fn)) self%entries(i)%fn => fn
    end subroutine extract_set_local

    ! Not a port.
    function current_extract(self) result(fn)
        class(imenu_extract_index_name_function_type), intent(in) :: self
        procedure(imenu_extract_index_name_procedure), pointer :: fn
        integer :: i

        fn => self%value
        if (.not. allocated(self%entries)) return
        i = findloc(self%entries%serial, current_serial(), dim=1)
        if (i > 0) fn => self%entries(i)%fn
    end function current_extract

    ! Not a port.
    subroutine extract_clear_local(self)
        class(imenu_extract_index_name_function_type), intent(inout) :: self
        integer :: i

        if (.not. allocated(self%entries)) return
        i = findloc(self%entries%serial, current_serial(), dim=1)
        if (i == 0) return
        self%entries(i)%serial = 0
        self%entries(i)%fn => null()
    end subroutine extract_clear_local
end module imenu_mod
