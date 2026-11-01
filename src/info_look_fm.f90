! info_look_fm.f90 -- major mode sensitive help agent.
! Ported from GNU Emacs 31.1 lisp/info-look.el.
!
! Copyright (C) 1995-1999, 2001-2026 Free Software Foundation, Inc.
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

!> Looking up identifiers and files in the Info manuals of the buffer's
!> major mode: `info_lookup_name' (C-h S), `info_lookup_file' and
!> `info_lookup_reset'.  The manuals of each mode are help
!> specifications added with `info_lookup_add_help' (the modes of
!> upstream's info-look.el are added by `syms_of_info_look'): the index
!> nodes to read the documented items from, and how to find an item in
!> the text of its node.
!>
!> Deferred: `info_complete_name' and `info_complete_file' (completion
!> in the buffer), the obsolete `info_lookup_guess_custom_name', and
!> raising another frame that shows *info*.
module info_look_fm
    use, intrinsic :: iso_fortran_env, only: int64, int32
    use callint_fm, only: PREFIX_NONE
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: xsymbol, symbols, value_type, make_list, vtag, V_NULL, V_NAME
    use casefiddle_fm, only: downcase_string
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer, get_buffer_create, is_buffer_live
    use editfns_fm, only: point, point_min, goto_char, line_beginning_position, line_end_position
    use search_fm, only: looking_at, string_match, search_forward, re_search_forward, re_search_backward, &
                         match_beginning, match_end, regexp_quote, case_fold_search, SEARCH_NOERROR, &
                         string_index_to_offset
    use subr_fm, only: match_string_no_properties
    use syntax_fm, only: skip_chars_forward, skip_chars_backward, skip_syntax_backward
    use textprop_fm, only: get_text_property
    use itree_fm, only: make_overlay, move_overlay, overlay_put, is_overlay
    use data_fm, only: logical_setting, add_local_variables_killer
    use fileio_fm, only: file_name_nondirectory
    use derived_fm, only: major_mode_named, major_mode_type
    use font_core_fm, only: major_mode_of_current
    use callint_fm, only: current_prefix_arg
    use commands_fm, only: commands
    use keyboard_fm, only: sit_for
    use xdisp_fm, only: message
    use simple_fm, only: back_to_indentation
    use window_fm, only: select_window, get_buffer_window, current_window_configuration, set_window_configuration, &
                         window_configuration_type, switch_to_buffer_other_window, pop_to_buffer_same_window
    use frame_fm, only: selected_window, selected_frame
    use minibuf_fm, only: completion_ignore_case, enable_recursive_minibuffers
    use minibuf_table_fm, only: completion_list_table
    use minibuffer_fm, only: completing_read, REQUIRE_MATCH_YES
    use info_fm, only: info, info_mode, info_goto_node, info_follow_nearest_node, info_find_file, info_current_file, &
                       info_current_node, info_history_variable, set_info_history_variable, info_history_entry_type, &
                       info_history_list, info_node_ref_type, info_fontify_maximum_menu_size, &
                       INFO_FONTIFY_MAXIMUM_MENU_SIZE_NONE
    implicit none(type, external)
    private

    public :: info_lookup_add_help, info_lookup_maybe_add_help, info_lookup_name, info_lookup_file, &
              info_lookup_reset, info_lookup, info_lookup_guess_default, info_lookup_guess_c_name, &
              info_lookup_guess_gdb_script_name, info_lookup_mode, syms_of_info_look

    abstract interface
        !> A TRANS-FUNC of a documentation specification: the help item of
        !> the index ENTRY, FOUND false if it has none.  Not a port.
        recursive subroutine info_lookup_trans_proc(entry, item, found)
            implicit none(type, external)
            character(len=*), intent(in) :: entry
            character(len=:), allocatable, intent(out) :: item
            logical, intent(out) :: found
        end subroutine info_lookup_trans_proc

        !> A PARSE-RULE function: the help item at point, FOUND false if
        !> there is none.  Not a port.
        recursive subroutine info_lookup_parse_proc(item, found)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: item
            logical, intent(out) :: found
        end subroutine info_lookup_parse_proc
    end interface

    public :: info_lookup_trans_proc, info_lookup_parse_proc

    !> A documentation specification (INFO-NODE TRANS-FUNC PREFIX SUFFIX):
    !> the Info index `node' (with its file name part); the index entries
    !> are translated into help items by `trans', or by prepending
    !> `trans_prefix' to their first word, or else only the entries
    !> matching the mode's regexp are taken.  If `prefix' or `suffix' is
    !> allocated, the help item's node is searched for the first
    !> occurrence of the regular expression `PREFIX ITEM SUFFIX'.
    type, public :: info_lookup_doc_spec_type
        character(len=:), allocatable :: node, trans_prefix, prefix, suffix
        procedure(info_lookup_trans_proc), pointer, nopass :: trans => null()
    end type info_lookup_doc_spec_type

    !> A list of documentation specifications, built with `add'.
    type, public :: info_lookup_doc_specs_type
        type(info_lookup_doc_spec_type), allocatable :: specs(:)
    contains
        !> Add the specification of the index NODE.
        procedure :: add => doc_specs_add
    end type info_lookup_doc_specs_type

    abstract interface
        !> A `doc_spec_function': the documentation specifications of a
        !> mode, computed when the user asks for help in it.  Not a port.
        recursive subroutine info_lookup_doc_spec_proc(specs)
            import :: info_lookup_doc_specs_type
            implicit none(type, external)
            type(info_lookup_doc_specs_type), intent(out) :: specs
        end subroutine info_lookup_doc_spec_proc
    end interface

    public :: info_lookup_doc_spec_proc

    ! A help mode: a mode's name, with the prefix of the symbols it
    ! documents (upstream's (HELP-MODE . SYMBOL-PREFIX)).
    type :: help_mode_type
        character(len=:), allocatable :: mode, prefix
    end type help_mode_type

    ! The help data of a mode (HELP-MODE REGEXP IGNORE-CASE DOC-SPEC
    ! PARSE-RULE OTHER-MODES DOC-SPEC-FUNCTION).  An unallocated
    ! `parse_rule' and null `parse_fn' are nil.
    type :: mode_value_type
        type(help_mode_type) :: mode
        character(len=:), allocatable :: regexp
        logical :: ignore_case = .false.
        logical :: has_doc_spec = .false.
        type(info_lookup_doc_specs_type) :: doc_spec
        character(len=:), allocatable :: parse_rule
        procedure(info_lookup_parse_proc), pointer, nopass :: parse_fn => null()
        type(help_mode_type), allocatable :: other_modes(:)
        procedure(info_lookup_doc_spec_proc), pointer, nopass :: doc_spec_fn => null()
    end type mode_value_type

    ! A help topic (HELP-TOPIC . HELP-DATA), the newest mode first.
    type :: topic_type
        character(len=:), allocatable :: topic
        type(mode_value_type), allocatable :: modes(:)
    end type topic_type

    ! info-lookup-alist: the known help topics, the newest first.
    type(topic_type), allocatable :: info_lookup_table(:)

    ! A documented help item and its index entry (unallocated if the
    ! same as the item).
    type :: completion_type
        character(len=:), allocatable :: item, entry
    end type completion_type

    ! Whether a mode's cache is initialized: nil, t, or 0 (initialized
    ! but void).
    integer, parameter :: INIT_PENDING = 0, INIT_DONE = 1, INIT_VOID = 2

    ! The cached data of a mode (HELP-MODE INITIALIZED COMPLETIONS
    ! REFER-MODES).
    type :: mode_cache_type
        character(len=:), allocatable :: topic
        type(help_mode_type) :: mode
        integer :: initialized = INIT_PENDING
        type(completion_type), allocatable :: completions(:)
        type(help_mode_type), allocatable :: refer_modes(:)
    end type mode_cache_type

    ! info-lookup-cache: the data maintained automatically by the
    ! program.
    type(mode_cache_type), allocatable :: info_lookup_cache(:)

    ! A buffer's `info_lookup_mode'.
    type :: local_mode_type
        integer(int64) :: serial = 0
        character(len=:), allocatable :: mode
    end type local_mode_type

    type(local_mode_type), allocatable :: local_modes(:)

    !> True means pop up the Info buffer in another window.  Port of
    !> info-look.el info-lookup-other-window-flag.
    type(logical_setting), public :: info_lookup_other_window_flag = &
                                     logical_setting(name="info_lookup_other_window_flag", value=.true.)

    !> Face for highlighting looked up help items.  Setting this variable
    !> to "" disables highlighting.  Port of info-look.el
    !> info-lookup-highlight-face.
    character(len=:), allocatable, public :: info_lookup_highlight_face

    !> Table of file names handled specially.  If a file name matches
    !> `regexp', then use help mode `mode' instead of the buffer's major
    !> mode.  Port of info-look.el info-lookup-file-name-alist.
    type, public :: info_lookup_file_name_type
        character(len=64) :: regexp = ""
        character(len=64) :: mode = ""
    end type info_lookup_file_name_type

    !> See the type.  Port of info-look.el info-lookup-file-name-alist.
    type(info_lookup_file_name_type), allocatable, public :: info_lookup_file_name_table(:)

    ! The overlay highlighting the looked up item
    ! (info-lookup-highlight-overlay).
    type(handle_type) :: highlight_overlay

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)
    ! The characters of a Lisp symbol's name in the regexps of the Lisp
    ! modes ("[^][()`'‘’,\" \t\n]+").
    character(len=*), parameter :: LISP_SYMBOL_RE = "[^][()`'‘’,"" "//TAB//LF//"]+"

contains

    ! ------------------------------------------------------------------
    ! Help specifications
    ! ------------------------------------------------------------------

    !> Add the documentation specification of the Info index NODE: see
    !> `info_lookup_doc_spec_type' for TRANS_PREFIX, TRANS, PREFIX and
    !> SUFFIX.  Not a port.
    subroutine doc_specs_add(self, node, trans_prefix, trans, prefix, suffix)
        class(info_lookup_doc_specs_type), intent(inout) :: self
        character(len=*), intent(in) :: node
        character(len=*), intent(in), optional :: trans_prefix, prefix, suffix
        procedure(info_lookup_trans_proc), optional :: trans
        type(info_lookup_doc_spec_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(self%specs)) allocate (self%specs(0))
        n = size(self%specs)
        allocate (grown(n + 1))
        grown(1:n) = self%specs
        grown(n + 1)%node = node
        if (present(trans_prefix)) grown(n + 1)%trans_prefix = trans_prefix
        if (present(trans)) grown(n + 1)%trans => trans
        if (present(prefix)) grown(n + 1)%prefix = prefix
        if (present(suffix)) grown(n + 1)%suffix = suffix
        call move_alloc(grown, self%specs)
    end subroutine doc_specs_add

    !> Add or update a help specification.
    !> The arguments are the parts of the help data of a mode: help is
    !> about TOPIC (default "symbol") in MODE (default the current major
    !> mode), for the names beginning with SYMBOL_PREFIX and a dash if
    !> present; the help items match REGEXP (default "\w+"), case is
    !> ignored if IGNORE_CASE; DOC_SPEC lists the Info indexes to read,
    !> or DOC_SPEC_FUNCTION computes it when the user asks for info on
    !> the mode in question; PARSE_RULE (a regexp) or PARSE_FUNCTION guess
    !> the help item at point; OTHER_MODES are cross references to other
    !> help modes.  Port of info-look.el:info-lookup-add-help.
    subroutine info_lookup_add_help(topic, mode, symbol_prefix, regexp, ignore_case, doc_spec, doc_spec_function, &
                                    parse_rule, parse_function, other_modes)
        character(len=*), intent(in), optional :: topic, mode, symbol_prefix, regexp, parse_rule
        logical, intent(in), optional :: ignore_case
        type(info_lookup_doc_specs_type), intent(in), optional :: doc_spec
        procedure(info_lookup_doc_spec_proc), optional :: doc_spec_function
        procedure(info_lookup_parse_proc), optional :: parse_function
        character(len=*), intent(in), optional :: other_modes(:)

        call add_help_star(.false., topic, mode, symbol_prefix, regexp, ignore_case, doc_spec, doc_spec_function, &
                           parse_rule, parse_function, other_modes)
    end subroutine info_lookup_add_help

    !> Add a help specification if none is defined.  See the
    !> documentation of the function `info_lookup_add_help' for more
    !> details.  Port of info-look.el:info-lookup-maybe-add-help.
    subroutine info_lookup_maybe_add_help(topic, mode, symbol_prefix, regexp, ignore_case, doc_spec, &
                                          doc_spec_function, parse_rule, parse_function, other_modes)
        character(len=*), intent(in), optional :: topic, mode, symbol_prefix, regexp, parse_rule
        logical, intent(in), optional :: ignore_case
        type(info_lookup_doc_specs_type), intent(in), optional :: doc_spec
        procedure(info_lookup_doc_spec_proc), optional :: doc_spec_function
        procedure(info_lookup_parse_proc), optional :: parse_function
        character(len=*), intent(in), optional :: other_modes(:)

        call add_help_star(.true., topic, mode, symbol_prefix, regexp, ignore_case, doc_spec, doc_spec_function, &
                           parse_rule, parse_function, other_modes)
    end subroutine info_lookup_maybe_add_help

    ! Port of info-look.el:info-lookup-add-help*.
    subroutine add_help_star(maybe, topic, mode, symbol_prefix, regexp, ignore_case, doc_spec, doc_spec_function, &
                             parse_rule, parse_function, other_modes)
        logical, intent(in) :: maybe
        character(len=*), intent(in), optional :: topic, mode, symbol_prefix, regexp, parse_rule
        logical, intent(in), optional :: ignore_case
        type(info_lookup_doc_specs_type), intent(in), optional :: doc_spec
        procedure(info_lookup_doc_spec_proc), optional :: doc_spec_function
        procedure(info_lookup_parse_proc), optional :: parse_function
        character(len=*), intent(in), optional :: other_modes(:)
        type(mode_value_type) :: data
        type(topic_type), allocatable :: grown(:)
        type(mode_value_type), allocatable :: modes(:)
        character(len=:), allocatable :: t
        integer :: ti, mi, i, n

        t = "symbol"
        if (present(topic)) t = topic

        if (present(mode)) then
            data%mode%mode = mode
        else if (cur > 0) then
            data%mode%mode = major_mode_of_current()
        else
            data%mode%mode = "fundamental_mode"
        end if

        if (present(symbol_prefix)) data%mode%prefix = symbol_prefix
        data%regexp = "\w+"
        if (present(regexp)) data%regexp = regexp
        if (present(ignore_case)) data%ignore_case = ignore_case

        if (present(doc_spec)) then
            data%has_doc_spec = .true.
            data%doc_spec = doc_spec
        end if

        if (present(doc_spec_function)) data%doc_spec_fn => doc_spec_function
        if (present(parse_rule)) data%parse_rule = parse_rule
        if (present(parse_function)) data%parse_fn => parse_function
        allocate (data%other_modes(0))

        if (present(other_modes)) then
            deallocate (data%other_modes)
            allocate (data%other_modes(size(other_modes)))

            do i = 1, size(other_modes)
                data%other_modes(i)%mode = trim(other_modes(i))
            end do
        end if

        call find_mode_value(t, data%mode, ti, mi)
        if (maybe .and. mi > 0) return

        if (ti == 0) then
            if (.not. allocated(info_lookup_table)) allocate (info_lookup_table(0))
            n = size(info_lookup_table)
            allocate (grown(n + 1))
            grown(1)%topic = t
            allocate (grown(1)%modes(0))

            do i = 1, n
                call move_alloc(info_lookup_table(i)%topic, grown(i + 1)%topic)
                call move_alloc(info_lookup_table(i)%modes, grown(i + 1)%modes)
            end do

            call move_alloc(grown, info_lookup_table)
            ti = 1
        end if

        if (mi == 0) then
            n = size(info_lookup_table(ti)%modes)
            allocate (modes(n + 1))
            modes(1) = data

            do i = 1, n
                modes(i + 1) = info_lookup_table(ti)%modes(i)
            end do

            call move_alloc(modes, info_lookup_table(ti)%modes)
        else
            info_lookup_table(ti)%modes(mi) = data
        end if
    end subroutine add_help_star

    ! Whether help modes A and B are the same (equal).  Not a port.
    pure logical function same_mode(a, b)
        type(help_mode_type), intent(in) :: a, b

        same_mode = .false.
        if (.not. (allocated(a%mode) .and. allocated(b%mode))) return
        if (a%mode /= b%mode .or. len(a%mode) /= len(b%mode)) return
        if (allocated(a%prefix) .neqv. allocated(b%prefix)) return

        if (allocated(a%prefix)) then
            if (a%prefix /= b%prefix .or. len(a%prefix) /= len(b%prefix)) return
        end if

        same_mode = .true.
    end function same_mode

    ! The help mode of the mode named NAME.  Not a port.
    function plain_mode(name) result(m)
        character(len=*), intent(in) :: name
        type(help_mode_type) :: m

        m%mode = name
    end function plain_mode

    ! The indexes of TOPIC in `info_lookup_table' and of MODE in it (0
    ! for none): info-lookup->topic-value and info-lookup->mode-value.
    ! Port of info-look.el:info-lookup->topic-value and
    ! info-look.el:info-lookup->mode-value.
    subroutine find_mode_value(topic, mode, ti, mi)
        character(len=*), intent(in) :: topic
        type(help_mode_type), intent(in) :: mode
        integer, intent(out) :: ti, mi
        integer :: i

        ti = 0
        mi = 0
        if (.not. allocated(info_lookup_table)) return

        do i = 1, size(info_lookup_table)
            if (info_lookup_table(i)%topic == topic) then
                ti = i
                exit
            end if
        end do

        if (ti == 0) return

        do i = 1, size(info_lookup_table(ti)%modes)
            if (same_mode(info_lookup_table(ti)%modes(i)%mode, mode)) then
                mi = i
                return
            end if
        end do
    end subroutine find_mode_value

    ! Whether TOPIC has help data for MODE (info-lookup->mode-value).
    ! Port of info-look.el:info-lookup->mode-value.
    logical function has_mode_value(topic, mode)
        character(len=*), intent(in) :: topic
        type(help_mode_type), intent(in) :: mode
        integer :: ti, mi

        call find_mode_value(topic, mode, ti, mi)
        has_mode_value = mi > 0
    end function has_mode_value

    ! We have a dynamic doc-spec function: compute the doc specs of
    ! TOPIC's MODE.  Port of info-look.el:info-lookup--expand-info.
    recursive subroutine expand_info(topic, mode)
        character(len=*), intent(in) :: topic
        type(help_mode_type), intent(in) :: mode
        type(info_lookup_doc_specs_type) :: specs
        integer :: ti, mi

        call find_mode_value(topic, mode, ti, mi)
        if (mi == 0) return

        associate (v => info_lookup_table(ti)%modes(mi))
            if (.not. v%has_doc_spec .and. associated(v%doc_spec_fn)) then
                call v%doc_spec_fn(specs)
                v%doc_spec = specs
                v%has_doc_spec = .true.
                v%doc_spec_fn => null()
            end if
        end associate
    end subroutine expand_info

    ! Port of info-look.el:info-lookup->ignore-case.
    logical function ignore_case_of(topic, mode)
        character(len=*), intent(in) :: topic
        type(help_mode_type), intent(in) :: mode
        integer :: ti, mi

        call find_mode_value(topic, mode, ti, mi)
        ignore_case_of = .false.
        if (mi > 0) ignore_case_of = info_lookup_table(ti)%modes(mi)%ignore_case
    end function ignore_case_of

    ! Port of info-look.el:info-lookup->doc-spec (none if not HAS).
    subroutine doc_spec_of(topic, mode, specs, has)
        character(len=*), intent(in) :: topic
        type(help_mode_type), intent(in) :: mode
        type(info_lookup_doc_specs_type), intent(out) :: specs
        logical, intent(out) :: has
        integer :: ti, mi

        call find_mode_value(topic, mode, ti, mi)
        has = .false.
        allocate (specs%specs(0))
        if (mi == 0) return
        if (.not. info_lookup_table(ti)%modes(mi)%has_doc_spec) return
        has = .true.
        specs = info_lookup_table(ti)%modes(mi)%doc_spec
        if (.not. allocated(specs%specs)) allocate (specs%specs(0))
    end subroutine doc_spec_of

    ! Port of info-look.el:info-lookup->other-modes.
    subroutine other_modes_of(topic, mode, others)
        character(len=*), intent(in) :: topic
        type(help_mode_type), intent(in) :: mode
        type(help_mode_type), allocatable, intent(out) :: others(:)
        integer :: ti, mi

        call find_mode_value(topic, mode, ti, mi)

        if (mi > 0) then
            others = info_lookup_table(ti)%modes(mi)%other_modes
        else
            allocate (others(0))
        end if
    end subroutine other_modes_of

    ! ------------------------------------------------------------------
    ! The cache
    ! ------------------------------------------------------------------

    ! The index of TOPIC's MODE in `info_lookup_cache' (0 for none):
    ! Port of info-look.el:info-lookup->mode-cache.
    integer function mode_cache(topic, mode) result(k)
        character(len=*), intent(in) :: topic
        type(help_mode_type), intent(in) :: mode
        integer :: i

        k = 0
        if (.not. allocated(info_lookup_cache)) return

        do i = 1, size(info_lookup_cache)
            if (info_lookup_cache(i)%topic == topic .and. same_mode(info_lookup_cache(i)%mode, mode)) then
                k = i
                return
            end if
        end do
    end function mode_cache

    ! Port of info-look.el:info-lookup->initialized.
    integer function initialized(topic, mode)
        character(len=*), intent(in) :: topic
        type(help_mode_type), intent(in) :: mode
        integer :: k

        k = mode_cache(topic, mode)
        initialized = INIT_PENDING
        if (k > 0) initialized = info_lookup_cache(k)%initialized
    end function initialized

    ! The documented help items of TOPIC in MODE.  Port of
    ! info-look.el:info-lookup->completions.
    recursive subroutine completions_of(topic, mode, completions)
        character(len=*), intent(in) :: topic
        type(help_mode_type), intent(in) :: mode
        type(completion_type), allocatable, intent(out) :: completions(:)
        integer :: k, init

        if (initialized(topic, mode) == INIT_PENDING) init = info_lookup_setup_mode(topic, mode)
        k = mode_cache(topic, mode)

        if (k > 0) then
            if (allocated(info_lookup_cache(k)%completions)) then
                completions = info_lookup_cache(k)%completions
                return
            end if
        end if

        allocate (completions(0))
    end subroutine completions_of

    ! MODE followed by its other help modes in use.  Port of
    ! info-look.el:info-lookup->all-modes (with
    ! info-lookup->refer-modes).
    recursive subroutine all_modes(topic, mode, modes)
        character(len=*), intent(in) :: topic
        type(help_mode_type), intent(in) :: mode
        type(help_mode_type), allocatable, intent(out) :: modes(:)
        integer :: k, init, i, n

        if (initialized(topic, mode) == INIT_PENDING) init = info_lookup_setup_mode(topic, mode)
        k = mode_cache(topic, mode)
        n = 0

        if (k > 0) then
            if (allocated(info_lookup_cache(k)%refer_modes)) n = size(info_lookup_cache(k)%refer_modes)
        end if

        allocate (modes(n + 1))
        modes(1) = mode

        do i = 1, n
            modes(i + 1) = info_lookup_cache(k)%refer_modes(i)
        end do
    end subroutine all_modes

    !> Throw away all cached data.
    !> This command is useful if the user wants to start at the beginning
    !> without quitting Emacs, for example, after some Info documents
    !> were updated on the system.  Port of
    !> info-look.el:info-lookup-reset.
    subroutine info_lookup_reset(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (allocated(info_lookup_cache)) deallocate (info_lookup_cache)
    end subroutine info_lookup_reset

    ! ------------------------------------------------------------------
    ! Looking up
    ! ------------------------------------------------------------------

    !> The current buffer's help mode (unallocated when unset): help is
    !> provided according to the buffer's major mode if it is unallocated.  Port of
    !> info-look.el info-lookup-mode (buffer-local).
    function info_lookup_mode() result(mode)
        character(len=:), allocatable :: mode
        integer :: k

        k = local_slot(.false.)

        if (k > 0) then
            if (allocated(local_modes(k)%mode)) mode = local_modes(k)%mode
        end if
    end function info_lookup_mode

    ! (setq info-lookup-mode MODE).  Not a port.
    subroutine set_info_lookup_mode(mode)
        character(len=*), intent(in) :: mode
        integer :: k

        k = local_slot(.true.)
        if (k > 0) local_modes(k)%mode = mode
    end subroutine set_info_lookup_mode

    ! The current buffer's entry in `local_modes', made if CREATE (0 if
    ! none).  Not a port.
    integer function local_slot(create) result(k)
        logical, intent(in) :: create
        type(local_mode_type), allocatable :: grown(:)
        integer :: i, n

        k = 0
        if (cur == 0) return
        if (.not. allocated(local_modes)) allocate (local_modes(0))

        do i = 1, size(local_modes)
            if (local_modes(i)%serial == buffers(cur)%serial) then
                k = i
                return
            end if
        end do

        if (.not. create) return

        do i = 1, size(local_modes)
            if (local_modes(i)%serial == 0) then
                k = i
                local_modes(k)%serial = buffers(cur)%serial
                return
            end if
        end do

        n = size(local_modes)
        allocate (grown(n + 1))
        grown(1:n) = local_modes
        grown(n + 1)%serial = buffers(cur)%serial
        call move_alloc(grown, local_modes)
        k = n + 1
    end function local_slot

    ! Forget the current buffer's `info_lookup_mode'
    ! (kill-all-local-variables).  Not a port.
    subroutine kill_info_look_locals()
        integer :: k

        k = local_slot(.false.)
        if (k > 0) local_modes(k) = local_mode_type()
    end subroutine kill_info_look_locals

    !> Look up and display documentation of SYMBOL in the relevant Info
    !> manual.  SYMBOL should be an identifier: a function or method, a
    !> macro, a variable, a data type, a class, etc.
    !>
    !> Interactively, prompt for SYMBOL; you can use
    !> \<minibuffer_local_completion_map>\[next_history_element] in the
    !> minibuffer to yank the default argument value into the minibuffer
    !> so you can edit it.  The default identifier is the one found at point.
    !>
    !> MODE is the major mode whose Info manuals to search for the
    !> documentation of SYMBOL.  It defaults to the current buffer's
    !> `major_mode'; if that mode doesn't have any Info manuals known to
    !> Emacs, the command will prompt for MODE to use, with completion.
    !> With prefix arg, the command always prompts for MODE.
    !>
    !> Is SAME_WINDOW, try to reuse the current window instead of popping
    !> up a new one.  Port of info-look.el:info-lookup-symbol.
    recursive subroutine info_lookup_name(symbol, status, mode, same_window)
        character(len=*), intent(in) :: symbol
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: mode
        logical, intent(in), optional :: same_window

        call info_lookup("symbol", symbol, status, mode, same_window)
    end subroutine info_lookup_name

    !> Look up and display documentation of FILE in the relevant Info
    !> manual.  FILE should be the name of a file; a notable example is a
    !> standard header file that is part of the C or C++ standard
    !> library.
    !>
    !> Interactively, prompt for FILE; you can use
    !> \<minibuffer_local_completion_map>\[next_history_element] in the
    !> minibuffer to yank the default argument value into the minibuffer
    !> so you can edit it.  The default file name is the one found at
    !> point.
    !>
    !> MODE is the major mode whose Info manuals to search for the
    !> documentation of FILE.  It defaults to the current buffer's
    !> `major_mode'; if that mode doesn't have any Info manuals known to
    !> Emacs, the command will prompt for MODE to use, with completion.
    !> With prefix arg, the command always prompts for MODE.  Port of
    !> info-look.el:info-lookup-file.
    recursive subroutine info_lookup_file(file, status, mode)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: mode

        call info_lookup("file", file, status, mode)
    end subroutine info_lookup_file

    recursive subroutine cmd_info_lookup_name(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: value, mode

        call interactive_arguments("symbol", current_prefix_arg%kind /= PREFIX_NONE, value, mode, status)
        if (.not. status%ok()) return
        call info_lookup_name(value, status, mode)
    end subroutine cmd_info_lookup_name

    recursive subroutine cmd_info_lookup_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: value, mode

        call interactive_arguments("file", current_prefix_arg%kind /= PREFIX_NONE, value, mode, status)
        if (.not. status%ok()) return
        call info_lookup_file(value, status, mode)
    end subroutine cmd_info_lookup_file

    subroutine cmd_info_lookup_reset(status)
        type(status_type), intent(out) :: status

        call info_lookup_reset(status)
    end subroutine cmd_info_lookup_reset

    ! Read and return argument VALUE (and help MODE) for help TOPIC.
    ! TOPIC should be any known symbol of a help topic, such as "file" or
    ! "symbol".  If QUERY is true, query for the help mode.  Port of
    ! info-look.el:info-lookup-interactive-arguments.
    recursive subroutine interactive_arguments(topic, query, value, mode, status)
        character(len=*), intent(in) :: topic
        logical, intent(in) :: query
        character(len=:), allocatable, intent(out) :: value, mode
        type(status_type), intent(out) :: status
        type(completion_type), allocatable :: completions(:)
        character(len=:), allocatable :: default, selected, prompt
        logical :: has_default, saved_ignore, saved_recursive

        status = status_type()

        if (query) then
            call info_lookup_change_mode(topic, mode, status)
        else
            selected = info_lookup_select_mode()

            if (has_mode_value(topic, plain_mode(selected))) then
                call expand_info(topic, plain_mode(selected))
                mode = info_lookup_mode()
            else
                call info_lookup_change_mode(topic, mode, status)
            end if
        end if

        if (.not. status%ok()) return
        call completions_of(topic, plain_mode(mode), completions)
        call info_lookup_guess_default(topic, mode, default, has_default)
        saved_ignore = completion_ignore_case%value
        saved_recursive = enable_recursive_minibuffers
        completion_ignore_case = ignore_case_of(topic, plain_mode(mode))
        enable_recursive_minibuffers = .true.

        if (has_default) then
            prompt = "Describe "//topic//" (default "//default//"): "
            call completing_read(prompt, completion_list_table(items_value(completions)), value, status, &
                                 history="info_lookup_history", default=default)
        else
            prompt = "Describe "//topic//": "
            call completing_read(prompt, completion_list_table(items_value(completions)), value, status, &
                                 history="info_lookup_history")
        end if

        completion_ignore_case = saved_ignore
        enable_recursive_minibuffers = saved_recursive
        if (.not. status%ok()) return
        if (len(value) == 0 .and. has_default) value = default
    end subroutine interactive_arguments

    ! The help items of COMPLETIONS as a list value.  Not a port.
    function items_value(completions) result(v)
        type(completion_type), intent(in) :: completions(:)
        type(value_type) :: v
        type(value_type), allocatable :: elems(:)
        integer :: i

        allocate (elems(size(completions)))

        do i = 1, size(completions)
            elems(i) = value_type(text=completions(i)%item)
        end do

        v = make_list(elems)
    end function items_value

    ! The help mode of the current buffer: the mode of its file name in
    ! `info_lookup_file_name_table', else the major mode or the first of
    ! its parents with help data.  Sets `info_lookup_mode'.  Port of
    ! info-look.el:info-lookup-select-mode (the parents by
    ! `major_mode_type' parents).
    function info_lookup_select_mode() result(mode)
        character(len=:), allocatable :: mode
        character(len=:), allocatable :: file_name
        type(major_mode_type), pointer :: m
        type(status_type) :: st
        integer :: i, t

        mode = info_lookup_mode()

        if (.not. allocated(mode) .and. allocated(buffers(cur)%filename)) then
            file_name = file_name_nondirectory(buffers(cur)%filename)

            if (allocated(info_lookup_file_name_table)) then
                do i = 1, size(info_lookup_file_name_table)
                    if (string_match(trim(info_lookup_file_name_table(i)%regexp), file_name, st) > 0) then
                        call set_info_lookup_mode(trim(info_lookup_file_name_table(i)%mode))
                        exit
                    end if
                end do
            end if
        end if

        ! If major-mode has no setups in info-lookup-alist, under any
        ! topic, then search up through its parents to find a parent mode
        ! which does have some setups.  This means that a derived mode
        ! with no setups of its own will select its parent mode for
        ! lookups, if one of its parents has some setups.
        mode = info_lookup_mode()

        if (.not. allocated(mode)) then
            m => major_mode_named(major_mode_of_current())

            do while (associated(m))
                if (allocated(info_lookup_table)) then
                    do t = 1, size(info_lookup_table)
                        if (has_mode_value(info_lookup_table(t)%topic, plain_mode(trim(m%name)))) then
                            call set_info_lookup_mode(trim(m%name))
                        end if
                    end do
                end if

                mode = info_lookup_mode()
                if (allocated(mode)) exit
                m => m%parent
            end do
        end if

        mode = info_lookup_mode()

        if (.not. allocated(mode)) then
            call set_info_lookup_mode(major_mode_of_current())
            mode = major_mode_of_current()
        end if
    end function info_lookup_select_mode

    ! Read the major mode whose manuals to search for TOPIC.  Port of
    ! info-look.el:info-lookup-change-mode.
    recursive subroutine info_lookup_change_mode(topic, mode, status)
        character(len=*), intent(in) :: topic
        character(len=:), allocatable, intent(out) :: mode
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: elems(:)
        type(value_type) :: names
        character(len=:), allocatable :: input
        integer :: ti, mi, i

        call find_mode_value(topic, plain_mode(""), ti, mi)

        if (ti > 0) then
            allocate (elems(size(info_lookup_table(ti)%modes)))

            do i = 1, size(elems)
                elems(i) = value_type(text=info_lookup_table(ti)%modes(i)%mode%mode)
            end do
        else
            allocate (elems(0))
        end if

        names = make_list(elems)
        call completing_read("Major mode whose manuals to search for this "//topic//": ", &
                             completion_list_table(names), input, status, require_match=REQUIRE_MATCH_YES, &
                             history="info_lookup_history")
        if (.not. status%ok()) return
        mi = 0

        if (ti > 0) then
            do i = 1, size(info_lookup_table(ti)%modes)
                if (info_lookup_table(ti)%modes(i)%mode%mode == input) mi = i
            end do
        end if

        if (mi == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No manuals available for "//topic)
            return
        end if

        mode = input

        if (.not. has_mode_value(topic, plain_mode(mode))) then
            call set_error(status, FM_GENERIC_ERROR, "The manuals of `"//mode//"' have no "//topic//" help")
            return
        end if

        call set_info_lookup_mode(mode)
    end subroutine info_lookup_change_mode

    ! The help mode for ITEM in MODE: (MODE . PREFIX) if the symbols
    ! beginning with ITEM's prefix (up to its first dash) have help data
    ! of their own.  Port of info-look.el:info-lookup--item-to-mode.
    function item_to_mode(item, mode) result(m)
        character(len=*), intent(in) :: item, mode
        type(help_mode_type) :: m
        type(help_mode_type) :: spec
        integer :: dash

        spec%mode = mode
        dash = index(item, "-")

        if (dash > 0) then
            spec%prefix = item(1:dash - 1)
        else
            spec%prefix = item
        end if

        if (has_mode_value("symbol", spec)) then
            m = spec
        else
            m%mode = mode
        end if
    end function item_to_mode

    !> Display the documentation of TOPIC whose name is ITEM, using MODE's
    !> manuals.  TOPIC should be any known symbol of a help topic type,
    !> such as "file" or "symbol".  ITEM is the item whose documentation
    !> to search: file name if TOPIC is "file", a symbol if TOPIC is
    !> "symbol", etc.  MODE is the `major_mode' whose Info manuals to
    !> search for documentation of ITEM; if it's absent, the function uses
    !> `info_lookup_file_name_table' and the current buffer's file name to
    !> guess the mode.
    !>
    !> If SAME_WINDOW, reuse the current window.  If false, pop to a
    !> different window.  Port of info-look.el:info-lookup.
    recursive subroutine info_lookup(topic, item, status, mode, same_window)
        character(len=*), intent(in) :: topic, item
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: mode
        logical, intent(in), optional :: same_window
        type(help_mode_type) :: hmode
        type(help_mode_type), allocatable :: modes(:)
        type(completion_type), allocatable :: completions(:)
        type(info_lookup_doc_specs_type) :: doc_spec
        type(info_history_entry_type), allocatable :: new_history(:), history(:)
        type(window_configuration_type) :: config
        type(handle_type) :: window, info_buffer, info_window, old
        type(status_type) :: st
        character(len=:), allocatable :: m, file, node, entry_item, entry_name, prefix, suffix
        logical :: ignore_case, found, doc_found, has_history, has, same, saved_fold, matched
        integer :: e, i, j, saved_size
        integer(int64) :: pos, start
        type(info_node_ref_type), allocatable :: saved_list(:)

        status = status_type()

        if (present(mode)) then
            m = mode
        else
            m = info_lookup_select_mode()
        end if

        hmode = item_to_mode(item, m)
        call completions_of(topic, hmode, completions)
        ignore_case = ignore_case_of(topic, hmode)
        e = 0

        do i = 1, size(completions)
            if (ignore_case) then
                if (completions(i)%item == downcase(item)) e = i
            else if (completions(i)%item == item) then
                e = i
            end if

            if (e > 0) exit
        end do

        if (e == 0) then
            ! (assoc-string item completions t)
            do i = 1, size(completions)
                if (downcase(completions(i)%item) == downcase(item)) then
                    e = i
                    exit
                end if
            end do
        end if

        if (e == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Not documented as a "//topic//": "//item)
            return
        end if

        entry_item = completions(e)%item

        if (allocated(completions(e)%entry)) then
            entry_name = completions(e)%entry
        else
            entry_name = entry_item
        end if

        call all_modes(topic, hmode, modes)
        window = selected_window()
        ! Avoid clobbering Info-history with nodes searched during lookup.
        ! If lookup succeeds set `Info-history' to `new-Info-history'.
        has_history = .false.
        info_buffer = get_buffer("*info*")

        if (info_buffer%index /= 0) then
            old = current_buffer()
            call set_buffer(info_buffer, st)
            call info_history_variable(history)
            file = info_current_file()
            node = info_current_node()
            if (.not. allocated(file)) file = ""
            if (.not. allocated(node)) node = ""
            allocate (new_history(size(history) + 1))
            new_history(1)%file = file
            new_history(1)%node = node
            new_history(1)%pos = point()

            do i = 1, size(history)
                new_history(i + 1) = history(i)
            end do

            has_history = .true.
            call set_buffer(old, st)
        end if

        same = .false.
        if (present(same_window)) same = same_window

        if (major_mode_of_current() /= "info_mode") then
            if (.not. info_lookup_other_window_flag%value) then
                call info(status)
            else
                call current_window_configuration(config)
                call info(status)
                call set_window_configuration(config)
                if (.not. status%ok()) return
                info_buffer = get_buffer("*info*")

                if (same) then
                    call pop_to_buffer_same_window(info_buffer, status)
                else
                    ! (*info* visible in another frame is Deferred with
                    ! frames.)
                    info_window = get_buffer_window(info_buffer)
                    call switch_to_buffer_other_window(info_buffer, status)
                end if
            end if

            if (.not. status%ok()) return
        end if

        found = .false.
        doc_found = .false.

        do j = 1, size(modes)
            if (found) exit
            call doc_spec_of(topic, modes(j), doc_spec, has)

            do i = 1, size(doc_spec%specs)
                if (found) exit
                node = doc_spec%specs(i)%node
                ! Don't need Index menu fontifications here, and they slow
                ! down the lookup.
                saved_size = info_fontify_maximum_menu_size
                info_fontify_maximum_menu_size = INFO_FONTIFY_MAXIMUM_MENU_SIZE_NONE

                if (allocated(info_history_list)) then
                    saved_list = info_history_list
                    deallocate (info_history_list)
                end if

                call info_goto_node(node, st)
                info_fontify_maximum_menu_size = saved_size
                if (allocated(info_history_list)) deallocate (info_history_list)
                if (allocated(saved_list)) call move_alloc(saved_list, info_history_list)

                if (.not. st%ok()) then
                    call message("Cannot access Info node "//node)
                    call sit_for(1000)
                    cycle
                end if

                doc_found = .true.
                ! Don't use Info-menu, it forces case-fold-search to t
                saved_fold = case_fold_search%value
                case_fold_search = .false.
                call re_search_forward("^\* "//regexp_quote(entry_name)//":", pos, st)
                case_fold_search = saved_fold
                if (.not. st%ok()) cycle
                call info_follow_nearest_node(st)
                if (.not. st%ok()) cycle
                found = .true.
                if (.not. (allocated(doc_spec%specs(i)%prefix) .or. allocated(doc_spec%specs(i)%suffix))) cycle
                prefix = ""
                suffix = ""
                if (allocated(doc_spec%specs(i)%prefix)) prefix = doc_spec%specs(i)%prefix
                if (allocated(doc_spec%specs(i)%suffix)) suffix = doc_spec%specs(i)%suffix
                saved_fold = case_fold_search%value
                case_fold_search = ignore_case_of(topic, modes(j))
                call goto_char(point_min())
                call re_search_forward(prefix//regexp_quote(entry_item)//suffix, pos, st)
                case_fold_search = saved_fold
                if (.not. st%ok()) cycle
                call goto_char(match_beginning(0, st))

                if (is_display_color() .and. len(info_lookup_highlight_face) > 0) then
                    ! Search again for ITEM so that the first occurrence of
                    ! ITEM will be highlighted.
                    call re_search_forward(regexp_quote(entry_item), pos, st, noerror=SEARCH_NOERROR)
                    matched = st%ok() .and. pos > 0

                    if (matched) then
                        start = match_beginning(0, st)

                        if (is_overlay(highlight_overlay)) then
                            call move_overlay(highlight_overlay, start, match_end(0, st), st, current_buffer())
                        else
                            call make_overlay(highlight_overlay, start, match_end(0, st), st)
                        end if

                        call overlay_put(highlight_overlay, "face", value_type(name=info_lookup_highlight_face), st)
                    end if
                end if
            end do
        end do

        ! Alert the user if case was munged, and do this after bringing up
        ! the info buffer since that can print messages
        if (.not. ignore_case .and. item /= entry_item) call message("Found in different case: "//entry_item)
        if (found .and. has_history) call set_info_history_variable(new_history)

        if (.not. doc_found) then
            call set_error(status, FM_GENERIC_ERROR, "Info documentation for lookup was not found")
            return
        end if

        ! Don't leave the Info buffer if the help item couldn't be looked
        ! up.
        if (info_lookup_other_window_flag%value .and. found) call select_window(window, status)
    end subroutine info_lookup

    ! Whether the terminal can display colors (display-color-p).  Not a port.
    logical function is_display_color()
        is_display_color = selected_frame%tty%tn_max_colors > 0
    end function is_display_color

    ! STR in lower case (downcase).  Not a port.
    function downcase(str) result(r)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: r

        r = downcase_string(str)
    end function downcase

    ! Initialize the internal data structure.  Port of
    ! info-look.el:info-lookup-setup-mode.
    recursive integer function info_lookup_setup_mode(topic, mode) result(init)
        character(len=*), intent(in) :: topic
        type(help_mode_type), intent(in) :: mode
        type(help_mode_type), allocatable :: others(:), refer(:), tmp(:)
        type(completion_type), allocatable :: completions(:), more(:), joined(:)
        type(info_node_ref_type), allocatable :: saved_list(:)
        type(mode_cache_type), allocatable :: grown(:)
        type(status_type) :: st
        integer :: i, k, n, sub

        init = initialized(topic, mode)
        if (init /= INIT_PENDING) return
        init = INIT_VOID
        allocate (refer(0), completions(0))

        if (allocated(info_history_list)) then
            saved_list = info_history_list
            deallocate (info_history_list)
        end if

        if (.not. has_mode_value(topic, mode)) then
            call message("No "//topic//" help available for `"//mode%mode//"'")
        else
            ! Recursively setup cross references.  But refer only to
            ! non-void modes.
            call other_modes_of(topic, mode, others)

            do i = 1, size(others)
                if (initialized(topic, others(i)) == INIT_PENDING) sub = info_lookup_setup_mode(topic, others(i))

                if (initialized(topic, others(i)) == INIT_DONE) then
                    n = size(refer)
                    allocate (tmp(n + 1))
                    tmp(1:n) = refer
                    tmp(n + 1) = others(i)
                    call move_alloc(tmp, refer)
                end if
            end do

            ! Build the full completion table.
            call info_lookup_make_completions(topic, mode, completions, st)

            if (.not. st%ok()) then
                deallocate (completions)
                allocate (completions(0))
            end if

            do i = 1, size(refer)
                call completions_of(topic, refer(i), more)
                n = size(completions)
                allocate (joined(n + size(more)))
                joined(1:n) = completions
                joined(n + 1:) = more
                call move_alloc(joined, completions)
            end do

            init = INIT_DONE
        end if

        if (allocated(info_history_list)) deallocate (info_history_list)
        if (allocated(saved_list)) call move_alloc(saved_list, info_history_list)
        ! Update `info-lookup-cache'.
        k = mode_cache(topic, mode)

        if (k == 0) then
            if (.not. allocated(info_lookup_cache)) allocate (info_lookup_cache(0))
            n = size(info_lookup_cache)
            allocate (grown(n + 1))
            grown(1:n) = info_lookup_cache
            call move_alloc(grown, info_lookup_cache)
            k = n + 1
            info_lookup_cache(k)%topic = topic
            info_lookup_cache(k)%mode = mode
        end if

        info_lookup_cache(k)%initialized = init
        call move_alloc(completions, info_lookup_cache(k)%completions)
        call move_alloc(refer, info_lookup_cache(k)%refer_modes)
    end function info_lookup_setup_mode

    ! Create a unique table from all index entries.  Port of
    ! info-look.el:info-lookup-make-completions.
    recursive subroutine info_lookup_make_completions(topic, mode, result, status)
        character(len=*), intent(in) :: topic
        type(help_mode_type), intent(in) :: mode
        type(completion_type), allocatable, intent(out) :: result(:)
        type(status_type), intent(out) :: status
        type(info_lookup_doc_specs_type) :: doc_spec
        type(completion_type), allocatable :: grown(:)
        type(handle_type) :: buffer, old
        type(status_type) :: st
        character(len=:), allocatable :: regexp, node, entry, item, tprefix
        integer :: ti, mi, i, n, saved_size, kind
        integer(int64) :: pos
        logical :: has, doc_found, ok, dup, ignore
        integer, parameter :: TRANS_REGEXP = 0, TRANS_PREFIX = 1, TRANS_FUNCTION = 2

        status = status_type()
        allocate (result(0))
        call doc_spec_of(topic, mode, doc_spec, has)
        call find_mode_value(topic, mode, ti, mi)
        regexp = "^\("//info_lookup_table(ti)%modes(mi)%regexp//"\)\([ "//TAB//"].*\)?$"
        ignore = ignore_case_of(topic, mode)
        doc_found = .false.
        saved_size = info_fontify_maximum_menu_size
        info_fontify_maximum_menu_size = INFO_FONTIFY_MAXIMUM_MENU_SIZE_NONE
        old = current_buffer()
        call get_buffer_create(" temp-info-look", buffer, st)
        call set_buffer(buffer, st)
        call info_mode(st)

        do i = 1, size(doc_spec%specs)
            node = doc_spec%specs(i)%node

            if (associated(doc_spec%specs(i)%trans)) then
                kind = TRANS_FUNCTION
            else if (allocated(doc_spec%specs(i)%trans_prefix)) then
                kind = TRANS_PREFIX
                tprefix = doc_spec%specs(i)%trans_prefix
            else
                kind = TRANS_REGEXP
            end if

            call message("Processing Info node `"//node//"'...")
            call info_goto_node(node, st)

            if (.not. st%ok()) then
                call message("Cannot access Info node `"//node//"'")
                call sit_for(1000)
            else
                doc_found = .true.
                call goto_char(point_min())
                call search_forward(LF//"* Menu:", pos, st, noerror=SEARCH_NOERROR)

                if (st%ok() .and. pos > 0) then
                    do
                        call re_search_forward(LF//"\* \(.*\): ", pos, st, noerror=SEARCH_NOERROR)
                        if (.not. st%ok() .or. pos == 0) exit
                        entry = match_string_no_properties(1)

                        select case (kind)
                        case (TRANS_FUNCTION)
                            call doc_spec%specs(i)%trans(entry, item, ok)
                        case (TRANS_PREFIX)
                            ok = string_match("^\([^: "//TAB//LF//"]+\)", entry, st, inhibit_modify=.false.) > 0
                            if (ok) item = tprefix//match_string_no_properties(1, entry)
                        case default
                            ok = string_match(regexp, entry, st, inhibit_modify=.false.) > 0
                            if (ok) item = match_string_no_properties(1, entry)
                        end select

                        ! `trans' can return nil if the regexp doesn't
                        ! match.  Sometimes there's more than one Menu:
                        if (.not. ok) cycle
                        if (entry == "Menu") cycle
                        if (ignore) item = downcase(item)
                        dup = .false.

                        do n = 1, size(result)
                            if (result(n)%item == item .and. len(result(n)%item) == len(item)) then
                                dup = .true.
                                exit
                            end if
                        end do

                        if (dup) cycle
                        ! (push (cons item entry) result): the newest first.
                        n = size(result)
                        allocate (grown(n + 1))
                        grown(1)%item = item
                        if (entry /= item .or. len(entry) /= len(item)) grown(1)%entry = entry
                        grown(2:) = result
                        call move_alloc(grown, result)
                    end do
                end if
            end if

            call message("Processing Info node `"//node//"'...done")
        end do

        call set_buffer(old, st)
        info_fontify_maximum_menu_size = saved_size
        if (.not. doc_found) call set_error(status, FM_GENERIC_ERROR, "Info documentation for lookup was not found")
    end subroutine info_lookup_make_completions

    ! ------------------------------------------------------------------
    ! Guessing the item at point
    ! ------------------------------------------------------------------

    !> Return a GUESS for an identifier to look up, based on text around point.
    !> Try all related modes applicable to TOPIC and MODE.  FOUND is false
    !> if there is nothing appropriate in the buffer near point.  Port of
    !> info-look.el:info-lookup-guess-default.
    recursive subroutine info_lookup_guess_default(topic, mode, guess, found)
        character(len=*), intent(in) :: topic, mode
        character(len=:), allocatable, intent(out) :: guess
        logical, intent(out) :: found
        type(help_mode_type), allocatable :: modes(:)
        type(status_type) :: st
        integer(int64) :: pos, b
        integer :: i

        found = .false.
        call all_modes(topic, plain_mode(mode), modes)

        do i = 1, size(modes)
            call guess_default_star(topic, modes(i), guess, found)
            if (found) exit
        end do

        ! Collapse whitespace characters.
        if (found) then
            pos = 0

            do
                b = string_match("[ "//TAB//LF//"]+", guess, st, start=pos, inhibit_modify=.false.)
                if (b == 0) exit
                pos = b
                guess = replace_match_in(guess, b, match_end(0, st))
            end do
        end if

    contains

        ! GUESS with the characters from index B to E (0-based, end
        ! exclusive) replaced by a space.
        function replace_match_in(s, bidx, eidx) result(r)
            character(len=*), intent(in) :: s
            integer(int64), intent(in) :: bidx, eidx
            character(len=:), allocatable :: r
            integer(int64) :: ob, oe

            ob = string_index_to_offset(s, bidx)
            oe = string_index_to_offset(s, eidx)
            r = s(1:ob)//" "//s(oe + 1:)
        end function replace_match_in
    end subroutine info_lookup_guess_default

    ! Port of info-look.el:info-lookup-guess-default*.
    recursive subroutine guess_default_star(topic, mode, result, found)
        character(len=*), intent(in) :: topic
        type(help_mode_type), intent(in) :: mode
        character(len=:), allocatable, intent(out) :: result
        logical, intent(out) :: found
        character(len=:), allocatable :: regexp
        type(status_type) :: st
        integer(int64) :: start, end, pos, moved
        integer :: ti, mi, subexp
        logical :: saved_fold

        found = .false.
        call find_mode_value(topic, mode, ti, mi)
        if (mi == 0) return
        saved_fold = case_fold_search%value
        case_fold_search = ignore_case_of(topic, mode)
        start = point()

        associate (v => info_lookup_table(ti)%modes(mi))
            if (associated(v%parse_fn)) then
                call v%parse_fn(result, found)
            else
                if (allocated(v%parse_rule)) then
                    regexp = v%parse_rule
                else
                    regexp = v%regexp
                end if

                subexp = 0

                ! If at start of symbol, don't go back to end of previous
                ! one.
                if (looking_at("[ "//TAB//LF//"]", st, inhibit_modify=.true.)) then
                    moved = skip_chars_backward(" "//TAB//LF, st)
                end if

                end = point()

                do
                    call re_search_backward(regexp, pos, st, noerror=SEARCH_NOERROR)
                    if (.not. st%ok() .or. pos == 0) exit
                    if (.not. looking_at(regexp, st)) exit
                    if (match_end(0, st) < end) exit
                    result = match_string_no_properties(subexp)
                    found = .true.
                end do

                if (.not. found) then
                    call goto_char(start)
                    moved = skip_chars_forward(" "//TAB//LF, st)

                    if (looking_at(regexp, st)) then
                        result = match_string_no_properties(subexp)
                        found = .true.
                    end if
                end if
            end if
        end associate

        call goto_char(start)
        case_fold_search = saved_fold
    end subroutine guess_default_star

    !> Get the C identifier at point.  Port of
    !> info-look.el:info-lookup-guess-c-symbol.
    recursive subroutine info_lookup_guess_c_name(item, found)
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found
        character(len=:), allocatable :: prefix, name
        type(status_type) :: st
        integer(int64) :: start, moved
        logical :: has_prefix, has_name

        found = .false.
        moved = skip_syntax_backward("w_")
        start = point()
        ! Test for a leading `struct', `union', or `enum' keyword but
        ! ignore names like `foo_struct'.
        has_prefix = .false.

        if (skip_chars_backward(" "//TAB//LF, st) < 0) then
            if (skip_chars_backward("_a-zA-Z0-9", st) < 0) then
                if (looking_at("\(struct\|union\|enum\)\s ", st)) then
                    prefix = match_string_no_properties(1)//" "
                    has_prefix = .true.
                end if
            end if
        end if

        call goto_char(start)
        has_name = .false.

        if (looking_at("[_a-zA-Z][_a-zA-Z0-9]*", st)) then
            name = match_string_no_properties(0)
            has_name = .true.
        end if

        ! Caveat!  Look forward if point is at `struct' etc.
        if (.not. has_prefix .and. has_name) then
            if (name == "struct" .or. name == "union" .or. name == "enum") then
                if (looking_at("[a-z]+\s +\([_a-zA-Z][_a-zA-Z0-9]*\)", st)) then
                    prefix = name//" "
                    has_prefix = .true.
                    name = match_string_no_properties(1)
                end if
            end if
        end if

        if (has_prefix .or. has_name) then
            item = ""
            if (has_prefix) item = prefix
            if (has_name) item = item//name
            found = .true.
        end if
    end subroutine info_lookup_guess_c_name

    !> Get identifier at point in GDB script buffers.  Port of
    !> info-look.el:info-lookup-guess-gdb-script-symbol.
    recursive subroutine info_lookup_guess_gdb_script_name(item, found)
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found
        character(len=:), allocatable :: line, str
        character(len=64), allocatable :: words(:)
        type(completion_type), allocatable :: completions(:)
        type(status_type) :: st
        integer(int64) :: start
        integer :: n, i, k

        found = .false.
        start = point()
        call back_to_indentation()

        ! Try to find the current line's full command in the index; and
        ! default to the longest subset that is found.
        if (looking_at("[-a-z]+\(\s-[-a-z]+\)*", st)) then
            line = match_string_no_properties(0)
            call split_words(line, words, n)
            call completions_of("symbol", plain_mode("gdb_script_mode"), completions)

            do while (n > 0)
                str = trim(words(1))

                do k = 2, n
                    str = str//" "//trim(words(k))
                end do

                do i = 1, size(completions)
                    if (completions(i)%item == str) then
                        item = str
                        found = .true.
                        exit
                    end if
                end do

                if (found) exit
                n = n - 1
            end do
        end if

        call goto_char(start)

    contains

        ! (split-string S "\\s-+" t)
        subroutine split_words(s, w, count)
            character(len=*), intent(in) :: s
            character(len=64), allocatable, intent(out) :: w(:)
            integer, intent(out) :: count
            integer :: p, q

            allocate (w(len(s)))
            count = 0
            p = 1

            do while (p <= len(s))
                if (s(p:p) == " " .or. s(p:p) == TAB) then
                    p = p + 1
                    cycle
                end if

                q = p

                do while (q <= len(s))
                    if (s(q:q) == " " .or. s(q:q) == TAB) exit
                    q = q + 1
                end do

                count = count + 1
                w(count) = s(p:q - 1)
                p = q
            end do
        end subroutine split_words
    end subroutine info_lookup_guess_gdb_script_name

    ! The parse rule `ignore': no item.  Not a port.
    recursive subroutine parse_ignore(item, found)
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found

        found = .false.
        item = ""
    end subroutine parse_ignore

    ! The parse rule of Custom mode: the symbol of the `custom_data'
    ! property at point.  Not a port.
    recursive subroutine parse_custom(item, found)
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found
        type(value_type) :: v
        type(status_type) :: st

        call get_text_property(point(), value_type(name="custom_data"), v, st)
        found = (vtag(v) == V_NAME .or. vtag(v) == V_NULL) .and. allocated(v%a)
        if (found) item = trim(symbols(xsymbol(v))%name)
    end subroutine parse_custom

    ! ------------------------------------------------------------------
    ! The translations of index entries of the modes
    ! ------------------------------------------------------------------

    ! Whether REGEXP matches STR (case-sensitively if EXACT), setting the
    ! match data.  Not a port.
    logical function smatch(regexp, str, exact)
        character(len=*), intent(in) :: regexp, str
        logical, intent(in), optional :: exact
        type(status_type) :: st
        logical :: saved

        saved = case_fold_search%value

        if (present(exact)) then
            if (exact) case_fold_search = .false.
        end if

        smatch = string_match(regexp, str, st, inhibit_modify=.false.) > 0
        case_fold_search = saved
    end function smatch

    ! Texinfo: ignore Emacs commands and prepend a `@'.  Not a port.
    recursive subroutine trans_texinfo(entry, item, found)
        character(len=*), intent(in) :: entry
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found

        found = smatch("^\([a-zA-Z]+\|[^a-zA-Z]\)\( .*\)?$", entry)
        if (found) item = "@"//match_string_no_properties(1, entry)
    end subroutine trans_texinfo

    ! Autoconf Macro Index entries are without an "AC_" prefix, but with
    ! "AH_" or "AU_" for those.  So add "AC_" if there isn't already an
    ! "A._".  Not a port.
    recursive subroutine trans_autoconf_macro(entry, item, found)
        character(len=*), intent(in) :: entry
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found

        found = .true.

        if (smatch("^A._", entry)) then
            item = entry
        else
            item = "AC_"//entry
        end if
    end subroutine trans_autoconf_macro

    ! M4 Macro Index entries are without "AS_" prefixes, and mostly
    ! without "m4_" prefixes.  "dnl" is an exception, not wanting any
    ! prefix.  So AS_ is added back to upper-case names (if needed), m4_
    ! to others which don't already an m4_.  Not a port.
    recursive subroutine trans_autoconf_m4(entry, item, found)
        character(len=*), intent(in) :: entry
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found

        found = .true.

        if (entry == "dnl" .or. smatch("^m4_", entry, .true.) .or. smatch("^AS_", entry, .true.)) then
            ! Autoconf 2.62 index includes some macros (e.g.,
            ! AS_HELP_STRING), so avoid prefixing.
            item = entry
        else if (smatch("^[A-Z0-9_]+$", entry, .true.)) then
            item = "AS_"//entry
        else
            item = "m4_"//entry
        end if
    end subroutine trans_autoconf_m4

    ! The items of the gawk index for awk mode.  Not a port.
    recursive subroutine trans_awk(entry, item, found)
        character(len=*), intent(in) :: entry
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found

        found = .false.

        if (smatch("^\([A-Z]+\) special pattern\b", entry, .true.)) then
            ! `BEGIN' and `END'.
            item = match_string_no_properties(1, entry)
            found = .true.
        else if (smatch("^\([a-z]+\) statement\b", entry, .true.)) then
            ! `if', `while', `do', ...
            if (match_string_no_properties(1, entry) /= "control") then
                item = match_string_no_properties(1, entry)
                found = .true.
            end if
        else if (smatch("^[A-Z]+$", entry, .true.)) then
            ! `NR', `NF', ...
            item = entry
            found = .true.
        else if (smatch("^[a-z]+$", entry, .true.)) then
            ! Built-in functions (matches to many entries).
            item = entry
            found = .true.
        end if
    end subroutine trans_awk

    ! Perl functions.  Not a port.
    recursive subroutine trans_perl_function(entry, item, found)
        character(len=*), intent(in) :: entry
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found

        found = smatch("^\([a-zA-Z0-9]+\)", entry)
        if (found) item = match_string_no_properties(1, entry)
    end subroutine trans_perl_function

    ! Perl variables: work around bad formatted array variables.  Not a port.
    recursive subroutine trans_perl_variable(entry, item, found)
        character(len=*), intent(in) :: entry
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found
        character(len=:), allocatable :: s
        integer :: k

        if (smatch("^\$\(.\|@@\)$", entry) .or. smatch("^\$\^[A-Z]$", entry)) then
            s = entry
        else if (smatch("^\([$%@]\|@@\)?[_a-zA-Z0-9]+", entry)) then
            s = match_string_no_properties(0, entry)
        else
            s = ""
        end if

        k = index(s, "@@")
        if (k > 0) s = s(1:k - 1)//s(k + 1:)
        found = len(s) > 0
        if (found) item = s
    end subroutine trans_perl_variable

    ! Octave: catch lines of the form "xyz statement".  Not a port.
    recursive subroutine trans_octave_concept(entry, item, found)
        character(len=*), intent(in) :: entry
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found

        found = smatch("^\([A-Z]+\) statement\b", entry)
        if (found) item = match_string_no_properties(1, entry)
    end subroutine trans_octave_concept

    ! coreutils: exclude extraneous stuff (most of it) by demanding
    ! "[a-z]+".  Not a port.
    recursive subroutine trans_coreutils(entry, item, found)
        character(len=*), intent(in) :: entry
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found

        found = smatch("\`[a-z]+\'", entry)
        if (found) item = entry
    end subroutine trans_coreutils

    ! diff: index entries are things like "foo invocation".  Not a port.
    recursive subroutine trans_diff(entry, item, found)
        character(len=*), intent(in) :: entry
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found

        found = smatch("\`\([a-z]+\) invocation\'", entry)
        if (found) item = match_string_no_properties(1, entry)
    end subroutine trans_diff

    ! sed: there's no plain "sed" index entry as such, mung another
    ! hopefully unique one to get to the invocation section.  Not a port.
    recursive subroutine trans_sed(entry, item, found)
        character(len=*), intent(in) :: entry
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found

        found = entry == "Standard input, processing as input"
        if (found) item = "sed"
    end subroutine trans_sed

    ! gawk for sh mode: there's no plain "awk" or "gawk" index entries,
    ! mung other hopefully unique ones to get to the command line options.
    ! Not a port.
    recursive subroutine trans_gawk_sh(entry, item, found)
        character(len=*), intent(in) :: entry
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found

        found = .true.

        if (entry == "gawk, extensions, disabling") then
            item = "awk"
        else if (entry == "gawk, versions of, information about, printing") then
            item = "gawk"
        else
            found = .false.
        end if
    end subroutine trans_gawk_sh

    ! cfengine: index entries may be like `IsPlain()'.  Not a port.
    recursive subroutine trans_cfengine(entry, item, found)
        character(len=*), intent(in) :: entry
        character(len=:), allocatable, intent(out) :: item
        logical, intent(out) :: found

        found = .true.

        if (smatch("\([[:alnum:]_]+\)()", entry)) then
            item = match_string_no_properties(1, entry)
        else
            item = entry
        end if
    end subroutine trans_cfengine

    ! Python: Debian includes Python info files, but they're
    ! version-named instead of having a symlink.  Python is released
    ! annually (PEP 602).  Not a port.
    recursive subroutine python_doc_spec(specs)
        type(info_lookup_doc_specs_type), intent(out) :: specs
        character(len=:), allocatable :: manual, found
        character(len=16) :: name
        type(status_type) :: st
        integer :: values(8), yy, version

        call date_and_time(values=values)
        yy = values(1) - 2000
        manual = "python"

        do version = yy, 7, -1
            write (name, "(a,i0)") "python3.", version
            call info_find_file(trim(name), found, st, noerror=.true.)

            if (st%ok() .and. allocated(found)) then
                manual = trim(name)
                exit
            end if
        end do

        call specs%add("("//manual//")Index")
        call specs%add("("//manual//")Python Module Index")
    end subroutine python_doc_spec

    ! ------------------------------------------------------------------
    ! Initialization
    ! ------------------------------------------------------------------

    !> Register the commands and add the help specifications of upstream's
    !> common modes ("Initialize some common modes" in info-look.el).
    !> Not a port.
    subroutine syms_of_info_look()
        type(info_lookup_doc_specs_type) :: s
        character(len=:), allocatable :: latex_node, found
        type(status_type) :: st

        commands%info_lookup_name%execute => cmd_info_lookup_name
        commands%info_lookup_name%info_file = "emacs"
        commands%info_lookup_file%execute => cmd_info_lookup_file
        commands%info_lookup_file%info_file = "emacs"
        commands%info_lookup_reset%execute => cmd_info_lookup_reset
        call add_local_variables_killer(kill_info_look_locals)
        if (.not. allocated(info_lookup_highlight_face)) info_lookup_highlight_face = "match"

        if (.not. allocated(info_lookup_file_name_table)) then
            allocate (info_lookup_file_name_table(1))
            info_lookup_file_name_table(1) = info_lookup_file_name_type(regexp="\`ac\(local\|site\|include\)\.m4\'", &
                                                                        mode="autoconf_mode")
        end if

        if (allocated(info_lookup_table)) return
        s = info_lookup_doc_specs_type()
        call s%add("(libc)Function Index", prefix="^[ "//TAB//"]+-+ \(Function\|Macro\): .*\<", suffix="\>")
        ! prefix/suffix has to match things like
        !   " -- Macro: int F_DUPFD"
        !   " -- Variable: char * tzname [2]"
        !   "`DBL_MAX'"    (texinfo @table)
        ! suffix "\\>" is not used because that sends DBL_MAX to
        ! DBL_MAX_EXP ("_" is a non-word char)
        call s%add("(libc)Variable Index", prefix="^\([ "//TAB//"]+-+ \(Variable\|Macro\): .*\<\|['`‘]\)", &
                   suffix="\( \|['’]?$\)")
        call s%add("(libc)Type Index", prefix="^[ "//TAB//"]+-+ Data Type: \<", suffix="\>")
        call s%add("(termcap)Var Index", prefix="^[ "//TAB//"]*['`‘]", suffix="['’]")
        call info_lookup_maybe_add_help(mode="c_mode", topic="symbol", &
                                        regexp="\(struct \|union \|enum \)?[_a-zA-Z][_a-zA-Z0-9]*", doc_spec=s, &
                                        parse_function=info_lookup_guess_c_name)
        s = info_lookup_doc_specs_type()
        call s%add("(libc)File Index")
        call info_lookup_maybe_add_help(mode="c_mode", topic="file", regexp="[_a-zA-Z0-9./+-]+", doc_spec=s)
        s = info_lookup_doc_specs_type()
        call s%add("(bison)Index", prefix="['`‘]", suffix="['’]")
        call info_lookup_maybe_add_help(mode="bison_mode", regexp="[:;|]\|%\([%{}]\|[_a-z]+\)\|YY[_A-Z]+\|yy[_a-z]+", &
                                        doc_spec=s, parse_rule="[:;|]\|%\([%{}]\|[_a-zA-Z][_a-zA-Z0-9]*\)", &
                                        other_modes=[character(len=16) :: "c_mode"])
        s = info_lookup_doc_specs_type()
        call s%add("(make)Name Index", prefix="^[ "//TAB//"]*['`‘]", suffix="['’]")
        call info_lookup_maybe_add_help(mode="makefile_mode", regexp="\$[^({]\|\.[_A-Z]*\|[_a-zA-Z][_a-zA-Z0-9-]*", &
                                        doc_spec=s, parse_rule="\$[^({]\|\.[_A-Z]*\|[_a-zA-Z0-9-]+")
        ! similar regexp/parse-rule as makefile-mode, but also the
        ! following (which have index entries), "##" special automake
        ! comment, "+=" append operator, separate from the GNU make one
        s = info_lookup_doc_specs_type()
        ! "(automake)Macro Index" is autoconf macros used in configure.ac,
        ! not Makefile.am, so don't have that here.
        call s%add("(automake)Variable Index", prefix="^[ "//TAB//"]*['`‘]", suffix="['’]")
        ! In automake 1.4 macros and variables were a combined node.
        call s%add("(automake)Macro and Variable Index", prefix="^[ "//TAB//"]*['`‘]", suffix="['’]")
        ! Directives like "if" are in the "General Index".  Prefix "`"
        ! since the text for say `+=' isn't always an @item etc and so not
        ! always at the start of a line.
        call s%add("(automake)General Index", prefix="['`‘]", suffix="['’]")
        ! In automake 1.3 there was just a single "Index" node.
        call s%add("(automake)Index", prefix="['`‘]", suffix="['’]")
        call info_lookup_maybe_add_help(topic="symbol", mode="makefile_automake_mode", &
                                        regexp="\$[^({]\|\.[_A-Z]*\|[_a-zA-Z][_a-zA-Z0-9-]*\|##\|\+=", &
                                        parse_rule="\$[^({]\|\.[_A-Z]*\|[_a-zA-Z0-9-]+\|##\|\+=", doc_spec=s, &
                                        other_modes=[character(len=16) :: "makefile_mode"])
        s = info_lookup_doc_specs_type()
        ! Ignore Emacs commands and prepend a `@'.
        call s%add("(texinfo)Command and Variable Index", trans=trans_texinfo, prefix="['`‘]", suffix="['’ ]")
        call info_lookup_maybe_add_help(mode="texinfo_mode", regexp="@\([a-zA-Z]+\|[^a-zA-Z]\)", doc_spec=s)
        s = info_lookup_doc_specs_type()
        call s%add("(m4)Macro index")
        call info_lookup_maybe_add_help(mode="m4_mode", regexp="[_a-zA-Z][_a-zA-Z0-9]*", doc_spec=s, &
                                        parse_rule="[_a-zA-Z0-9]+")
        s = info_lookup_doc_specs_type()
        call s%add("(autoconf)Autoconf Macro Index", trans=trans_autoconf_macro, &
                   prefix="^[ "//TAB//"]+-+ \(Macro\|Variable\): .*\<", suffix="\>")
        call s%add("(autoconf)M4 Macro Index", trans=trans_autoconf_m4, prefix="^[ "//TAB//"]+-+ Macro: .*\<", &
                   suffix="\>")
        ! Autotest Macro Index entries are without "AT_".
        call s%add("(autoconf)Autotest Macro Index", trans_prefix="AT_", prefix="^[ "//TAB//"]+-+ Macro: .*\<", &
                   suffix="\>")
        ! This is for older versions (probably pre autoconf 2.5x):
        call s%add("(autoconf)Macro Index", trans_prefix="AC_", &
                   prefix="^[ "//TAB//"]+-+ \(Macro\|Variable\): .*\<", suffix="\>")
        ! Automake has index entries for its notes on various autoconf
        ! macros (eg. AC_PROG_CC).  Ensure this is after the autoconf
        ! index, so as to prefer the autoconf docs.
        call s%add("(automake)Macro and Variable Index", prefix="^[ "//TAB//"]*['`‘]", suffix="['’]")
        ! Autoconf symbols are M4 macros.  Thus use M4's parser.
        call info_lookup_maybe_add_help(mode="autoconf_mode", regexp="A[CM]_[_A-Z0-9]+", doc_spec=s, &
                                        parse_function=parse_ignore, other_modes=[character(len=16) :: "m4_mode"])
        s = info_lookup_doc_specs_type()
        call s%add("(gawk)Index", trans=trans_awk, prefix="['`‘]", suffix="\([ "//TAB//"]*([^)]*)\)?['’]")
        call info_lookup_maybe_add_help(mode="awk_mode", regexp="[_a-zA-Z]+", doc_spec=s)
        s = info_lookup_doc_specs_type()
        call s%add("(perl5)Function Index", trans=trans_perl_function, prefix="^", suffix="\b")
        call s%add("(perl5)Variable Index", trans=trans_perl_variable, prefix="^", suffix="\b")
        call info_lookup_maybe_add_help(mode="perl_mode", regexp="[$@%][^a-zA-Z]\|\$\^[A-Z]\|[$@%]?[a-zA-Z][_a-zA-Z0-9]*", &
                                        doc_spec=s, parse_rule="[$@%]?\([_a-zA-Z0-9]+\|[^a-zA-Z]\)")
        call info_lookup_maybe_add_help(mode="python_mode", doc_spec_function=python_doc_spec)
        call info_lookup_maybe_add_help(mode="perl_mode", regexp="[$@%][^a-zA-Z]\|\$\^[A-Z]\|[$@%]?[a-zA-Z][_a-zA-Z0-9]*")
        s = info_lookup_doc_specs_type()
        ! From CTAN's https://ctan.org/pkg/latex2e-help-texinfo and
        ! https://puszcza.gnu.org.ua/projects/latexrefman/, else from
        ! https://savannah.nongnu.org/projects/latex-manual/.
        call info_find_file("latex2e", found, st, noerror=.true.)
        latex_node = "(latex-manual)Command Index"
        if (st%ok() .and. allocated(found)) latex_node = "(latex2e)Index"
        ! \frac{NUM}{DEN} etc can have more than one {xx} argument.
        ! \sqrt[ROOT]{num} and others can have square brackets.
        call s%add(latex_node, prefix="[`'‘]", suffix="\({[^}]*}|\[[^]]*\]\)*['’]")
        call info_lookup_maybe_add_help(mode="latex_mode", regexp="\\\([a-zA-Z]+\|[^a-zA-Z]\)", doc_spec=s)
        s = info_lookup_doc_specs_type()
        ! Commands with key sequences appear in nodes as `foo' and those
        ! without as `M-x foo'.
        call s%add("(emacs)Command Index", prefix="['`‘]\(M-x[ "//TAB//LF//"]+\)?", suffix="['’]")
        ! Variables normally appear in nodes as just `foo'.
        call s%add("(emacs)Variable Index", prefix="['`‘]", suffix="['’]")
        ! Almost all functions, variables, etc appear in nodes as " --
        ! Function: foo" etc.  A small number of aliases and symbols
        ! appear only as `foo', and will miss out on exact positions.
        ! Allowing `foo' would hit too many false matches for things that
        ! should go to Function: etc, and those latter are much more
        ! important.  Perhaps this could change if some sort of fallback
        ! match scheme existed.
        call s%add("(elisp)Index", prefix="^ -+ .*: ", suffix="\( \|$\)")
        call s%add("(cl)Function Index", prefix="^ -+ .*: ", suffix="\( \|$\)")
        call s%add("(cl)Variable Index", prefix="^ -+ .*: ", suffix="\( \|$\)")
        call info_lookup_maybe_add_help(mode="emacs_lisp_mode", regexp=LISP_SYMBOL_RE, doc_spec=s)
        s = info_lookup_doc_specs_type()
        call s%add("(elisp)Index", prefix="^ -+ .*: ", suffix="\( \|$\)")
        call s%add("(cl)Function Index", prefix="^ -+ .*: ", suffix="\( \|$\)")
        call s%add("(cl)Variable Index", prefix="^ -+ .*: ", suffix="\( \|$\)")
        call info_lookup_maybe_add_help(mode="emacs_lisp_only", regexp=LISP_SYMBOL_RE, doc_spec=s)
        ! Below we have a list of prefixes (used to match on symbols in
        ! `emacs_lisp_mode') and the nodes where the function/variable
        ! indices live.  If the prefix is different than the name of the
        ! manual, then the full "(manual)Node" name has to be used.
        call lisp_prefix("auth", [character(len=32) :: "Function Index", "Variable Index"])
        call lisp_prefix("autotype", [character(len=32) :: "Command Index", "Variable Index"])
        call lisp_prefix("calc", [character(len=32) :: "Lisp Function Index", "Variable Index"])
        call lisp_prefix("dbus", [character(len=32) :: "Index"])
        call lisp_prefix("ediff", [character(len=32) :: "Index"])
        call lisp_prefix("eieio", [character(len=32) :: "Function Index"])
        call lisp_prefix("gnutls", [character(len=32) :: "(emacs-gnutls)Variable Index", "(emacs-gnutls)Function Index"])
        call lisp_prefix("mm", [character(len=32) :: "(emacs-mime)Index"])
        call lisp_prefix("eglot", [character(len=32) :: "Index"])
        call lisp_prefix("epa", [character(len=32) :: "Variable Index", "Function Index"])
        call lisp_prefix("ert", [character(len=32) :: "Index"])
        call lisp_prefix("eshell", [character(len=32) :: "Function and Variable Index"])
        call lisp_prefix("eudc", [character(len=32) :: "Index"])
        call lisp_prefix("eww", [character(len=32) :: "Variable Index", "Lisp Function Index"])
        call lisp_prefix("flymake", [character(len=32) :: "Index"])
        call lisp_prefix("forms", [character(len=32) :: "Index"])
        call lisp_prefix("gnus", [character(len=32) :: "Index"])
        call lisp_prefix("htmlfontify", [character(len=32) :: "Functions", "Variables & Customization"])
        call lisp_prefix("idlwave", [character(len=32) :: "Index"])
        call lisp_prefix("ido", [character(len=32) :: "Variable Index", "Function Index"])
        call lisp_prefix("info", [character(len=32) :: "Index"])
        call lisp_prefix("mairix", [character(len=32) :: "(mairix-el)Variable Index", "(mairix-el)Function Index"])
        call lisp_prefix("message", [character(len=32) :: "Index"])
        call lisp_prefix("mh", [character(len=32) :: "(mh-e)Option Index", "(mh-e)Command Index"])
        call lisp_prefix("newsticker", [character(len=32) :: "Index"])
        call lisp_prefix("octave", [character(len=40) :: "(octave-mode)Variable Index", &
                                    "(octave-mode)Lisp Function Index"])
        call lisp_prefix("org", [character(len=32) :: "Variable Index", "Command and Function Index"])
        call lisp_prefix("rcirc", [character(len=32) :: "Variable Index", "Index"])
        call lisp_prefix("reftex", [character(len=32) :: "Index"])
        call lisp_prefix("sasl", [character(len=32) :: "Variable Index", "Function Index"])
        call lisp_prefix("sc", [character(len=32) :: "Variable Index"])
        call lisp_prefix("semantic", [character(len=32) :: "Index"])
        call lisp_prefix("ses", [character(len=32) :: "Index"])
        call lisp_prefix("sieve", [character(len=32) :: "Index"])
        call lisp_prefix("smtpmail", [character(len=32) :: "Function and Variable Index"])
        call lisp_prefix("srecode", [character(len=32) :: "Index"])
        call lisp_prefix("tramp", [character(len=32) :: "Variable Index", "Function Index"])
        call lisp_prefix("url", [character(len=32) :: "Variable Index", "Function Index"])
        call lisp_prefix("use-package", [character(len=32) :: "Index"])
        call lisp_prefix("vhdl", [character(len=32) :: "(vhdl-mode)Variable Index", "(vhdl-mode)Command Index"])
        call lisp_prefix("viper", [character(len=32) :: "Variable Index", "Function Index"])
        call lisp_prefix("vtable", [character(len=32) :: "Index"])
        call lisp_prefix("widget", [character(len=32) :: "Index"])
        call lisp_prefix("wisent", [character(len=32) :: "Index"])
        call lisp_prefix("woman", [character(len=32) :: "Variable Index", "Command Index"])
        ! docstrings talk about elisp, so have apropos-mode follow
        ! emacs-lisp-mode
        call info_lookup_maybe_add_help(mode="apropos_mode", regexp=LISP_SYMBOL_RE, &
                                        other_modes=[character(len=16) :: "emacs_lisp_mode"])
        call info_lookup_maybe_add_help(mode="lisp_interaction_mode", regexp=LISP_SYMBOL_RE, &
                                        parse_function=parse_ignore, other_modes=[character(len=16) :: "emacs_lisp_mode"])
        call info_lookup_maybe_add_help(mode="inferior_emacs_lisp_mode", &
                                        regexp="[^][()`'‘’,:"" "//TAB//LF//"]+", &
                                        other_modes=[character(len=16) :: "emacs_lisp_mode"])
        call info_lookup_maybe_add_help(mode="lisp_mode", regexp="[^()`'‘’,"" "//TAB//LF//"]+", &
                                        parse_function=parse_ignore, other_modes=[character(len=16) :: "emacs_lisp_mode"])
        s = info_lookup_doc_specs_type()
        ! Aubrey Jaffer's rendition from
        ! <https://people.csail.mit.edu/jaffer/SCM>
        call s%add("(r5rs)Index", prefix="^[ "//TAB//"]+-+ [^:]+:[ "//TAB//"]*", suffix="\b")
        call info_lookup_maybe_add_help(mode="scheme_mode", regexp="[^()`'‘’,"" "//TAB//LF//"]+", ignore_case=.true., &
                                        doc_spec=s)
        s = info_lookup_doc_specs_type()
        call s%add("(octave)Function Index", prefix="^ -+ [^:]+:[ ]+\(\[[^=]*=[ ]+\)?")
        call s%add("(octave)Variable Index", prefix="^ -+ [^:]+:[ ]+")
        call s%add("(octave)Operator Index")
        ! Catch lines of the form "xyz statement"
        call s%add("(octave)Concept Index", trans=trans_octave_concept)
        call info_lookup_maybe_add_help(mode="octave_mode", &
                                        regexp="[_a-zA-Z0-9]+\|\s.+\|[-!=^|*/.\,><~&+]\{1,3\}\|[][();,""']", &
                                        doc_spec=s)
        s = info_lookup_doc_specs_type()
        call s%add("(maxima)Function and Variable Index", prefix="^ -+ [^:]+:[ ]+\(\[[^=]*=[ ]+\)?")
        call info_lookup_maybe_add_help(mode="maxima_mode", ignore_case=.true., regexp="[a-zA-Z0-9_%]+", doc_spec=s)
        call info_lookup_maybe_add_help(mode="inferior_maxima_mode", regexp="[a-zA-Z0-9_%]+", &
                                        other_modes=[character(len=16) :: "maxima_mode"])
        ! coreutils and bash builtins overlap in places, eg. printf, so
        ! there's a question which should come first.  Some of the
        ! coreutils descriptions are more detailed, but if bash is usually
        ! /bin/sh on a GNU system then the builtins will be what's
        ! normally run.
        s = info_lookup_doc_specs_type()
        call s%add("(bash)Builtin Index", prefix="^['`‘]", suffix="[ .'’]")
        call s%add("(bash)Reserved Word Index", prefix="^['`‘]", suffix="[ .'’]")
        call s%add("(bash)Variable Index", prefix="^['`‘]", suffix="[ .'’]")
        ! coreutils (version 4.5.10) doesn't have a separate program
        ! index, so exclude extraneous stuff (most of it) by demanding
        ! "[a-z]+" in the trans-func.  coreutils version 8.1 has node
        ! "Concept Index" and past versions have node "Index", look for
        ! both, whichever is absent is quietly ignored
        call s%add("(coreutils)Index", trans=trans_coreutils)
        call s%add("(coreutils)Concept Index", trans=trans_coreutils)
        ! diff (version 2.8.1) has only a few programs, index entries are
        ! things like "foo invocation".
        call s%add("(diff)Index", trans=trans_diff)
        ! there's no plain "sed" index entry as such, mung another
        ! hopefully unique one to get to the invocation section
        call s%add("(sed)Concept Index", trans=trans_sed)
        ! there's no plain "awk" or "gawk" index entries, mung other
        ! hopefully unique ones to get to the command line options
        call s%add("(gawk)Index", trans=trans_gawk_sh)
        ! bash has "." and ":" in its index, but those chars will probably
        ! never work in info, so don't bother matching them in the regexp.
        call info_lookup_maybe_add_help(mode="sh_mode", topic="symbol", &
                                        regexp="\([a-zA-Z0-9_-]+\|[!{}@*#?$]\|\[\[?\|]]?\)", doc_spec=s)
        ! This misses some things which occur as node names but not in
        ! the index.  Unfortunately it also picks up the wrong one of
        ! multiple entries for the same term in some cases.  --fx
        s = info_lookup_doc_specs_type()
        ! This gets functions in evaluated classes.  Other possible
        ! patterns don't seem to work too well.
        call s%add("(cfengine-Reference)Variable Index", trans=trans_cfengine, prefix="['`‘]", suffix="(")
        call info_lookup_maybe_add_help(mode="cfengine_mode", regexp="[[:alnum:]_]+\(?:()\)?", doc_spec=s)
        call info_lookup_maybe_add_help(mode="Custom_mode", ignore_case=.true., &
                                        regexp="[^][()`'‘’,:"" "//TAB//LF//"]+", parse_function=parse_custom, &
                                        other_modes=[character(len=16) :: "emacs_lisp_mode"])
        call info_lookup_maybe_add_help(mode="help_mode", regexp="[^][()`'‘’,:"" "//TAB//LF//"]+", &
                                        other_modes=[character(len=16) :: "emacs_lisp_mode"])
        s = info_lookup_doc_specs_type()
        call s%add("(gdb)Command and Variable Index")
        call info_lookup_maybe_add_help(mode="gdb_script_mode", ignore_case=.false., &
                                        regexp="\([-a-z]+\(\s-+[-a-z]+\)*\)", doc_spec=s, &
                                        parse_function=info_lookup_guess_gdb_script_name)

    contains

        ! (info-lookup-add-help :mode (cons 'emacs-lisp-mode PREFIX) ...)
        ! with the index NODES of PREFIX's manual.
        subroutine lisp_prefix(prefix, nodes)
            character(len=*), intent(in) :: prefix
            character(len=*), intent(in) :: nodes(:)
            type(info_lookup_doc_specs_type) :: specs
            integer :: k

            specs = info_lookup_doc_specs_type()

            do k = 1, size(nodes)
                if (nodes(k) (1:1) == "(") then
                    call specs%add(trim(nodes(k)), prefix="^ -+ .*: ", suffix="\( \|$\)")
                else
                    call specs%add("("//prefix//")"//trim(nodes(k)), prefix="^ -+ .*: ", suffix="\( \|$\)")
                end if
            end do

            call info_lookup_add_help(mode="emacs_lisp_mode", symbol_prefix=prefix, &
                                      regexp="\b"//prefix//"-"//LISP_SYMBOL_RE, doc_spec=specs)
        end subroutine lisp_prefix
    end subroutine syms_of_info_look
end module info_look_fm
