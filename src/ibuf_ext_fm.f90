! ibuf_ext_fm.f90 -- extensions for ibuffer.
! Ported from GNU Emacs 31.1 lisp/ibuf-ext.el.
!
! Copyright (C) 2000-2026 Free Software Foundation, Inc.
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

!> These functions should be automatically loaded when called, but you
!> can explicitly (require 'ibuf-ext) in your ~/.emacs to have them
!> preloaded.  (Fortmacs builds them in: a submodule of ibuffer_fm, as
!> ibuf-ext.el and ibuffer.el use each other.)
!>
!> The filters are nested function calls (`ibuffer_and', `ibuffer_or',
!> `ibuffer_not', `ibuffer_saved' and the simple filters of ibuf_macs_fm):
!> a specification is its nodes in prefix order.  The filters of an
!> Ibuffer buffer are read with `ibuffer_filtering_qualifiers' and set
!> with `ibuffer_set_filtering_qualifiers', its filter groups likewise
!> (`ibuffer_filter_groups').  The commands `ibuffer_filter_by_NAME' that
!> define-ibuffer-filter makes are procedures of their own; the filters
!> themselves are entries of `ibuffer_filtering_table'.
!>
!> Deferred: saving `ibuffer_saved_filters' and
!> `ibuffer_saved_filter_groups' with `ibuffer_save_with_custom' (Customize,
!> step 26), `ibuffer_repair_saved_filters' and the old format of saved
!> filters, a fractional size read by `ibuffer_filter_by_size_gt' and
!> `ibuffer_filter_by_size_lt' (read as its integer part),
!> `ibuffer_do_isearch' and `ibuffer_do_isearch_regexp' (misearch.el),
!> `ibuffer_do_print' (lpr.el), the mouse commands
!> (`ibuffer_mouse_filter_by_mode', `ibuffer_mouse_toggle_filter_group');
!> remote files in `ibuffer_diff_with_file' (`diff-file-local-copy').  Not
!> applicable: `ibuffer_do_eval', `ibuffer_do_view_and_eval' and reading a
!> predicate form for `ibuffer_filter_by_predicate' (`/ e'; no run-time
!> Lisp: `ibuffer_filter_by_predicate' takes a Fortran function, shown as
!> " [predicate]" without its form).
submodule(ibuffer_fm) ibuf_ext_fm
    use data_fm, only: value_eq
    use simple_fm, only: execute_shell_region, execute_process_shell_command, shell_command_buffer_name, &
                         shell_command_dont_erase_buffer, kill_new, kill_append
    use keyboard_fm, only: last_command, post_command_hook
    use dispnew_fm, only: is_frame_or_buffer_changed, frame_buffer_state_type
    use minibuf_fm, only: read_from_minibuffer, is_minibuffer
    use minibuffer_fm, only: completing_read, read_buffer, REQUIRE_MATCH_YES
    use minibuf_table_fm, only: completion_table_type, TABLE_LIST
    use replace_fm, only: occur_read_primary_args, occur_1, occur_source_type, query_replace, query_replace_regexp, &
                          query_replace_read_args
    use search_fm, only: replace_match
    use fileio_fm, only: file_exists, file_name_nondirectory, file_name_directory, write_region
    use files_fm, only: file_relative_name, make_temp_file, revert_buffer
    use subr_fm, only: shell_quote_argument
    use diff_fm, only: diff_check_labels, diff_sentinel, diff_command, diff_switches, diff_use_labels, DIFF_LABELS_YES
    use diff_mode_fm, only: diff_mode
    use window_fm, only: pop_to_buffer_same_window
    use error_fm, only: FM_WRONG_TYPE_ARGUMENT
    use lisp_fm, only: xsymbol, symbols, list_elements, make_list, vtag, V_NULL, V_INTEGER, V_NAME, V_ARRAY, value_type
    use search_fm, only: SEARCH_NOERROR
    use files_fm, only: file_name_extension
    use simple_fm, only: simple_kill_line => kill_line
    use derived_fm, only: major_mode_type, major_mode_named, provided_mode_derived_p
    use minibuf_fm, only: minibuffer_string_type
    use crm_fm, only: completing_read_multiple
    use callint_fm, only: execute_interactively
    use keymap_fm, only: where_is_internal, current_local_map
    use minibuf_table_fm, only: TABLE_OBARRAY
    use ibuf_macs_fm, only: ibuffer_filter_node_type, ibuffer_filter, ibuffer_saved, ibuffer_predicate, &
                            ibuffer_filter_extent, ibuffer_filter_part, ibuffer_filter_equal, ibuffer_buffer_predicate_proc
    implicit none(type, external)

    ! frame-or-buffer-changed-p's state for `ibuffer_auto_mode'
    ! (ibuffer-auto-buffers-changed).
    type(frame_buffer_state_type) :: auto_buffers_changed

    ! The arguments of the operations being run (their interactive
    ! arguments).
    character(len=:), allocatable :: op_command, op_from, op_to
    logical :: op_delimited = .false., op_backward = .false.
    logical :: op_regexp = .false.

    ! The predicate and regexp of `ibuffer_mark_on_buffer'.
    procedure(ibuffer_buffer_predicate_proc), pointer :: mark_on_fn => null()
    character(len=:), allocatable :: mark_on_regexp
    logical :: mark_on_all = .false.

contains

    ! ------------------------------------------------------------------
    ! Visibility and groups
    ! ------------------------------------------------------------------

    module procedure is_ibuffer_ext_visible
    type(ibuffer_filter_type), allocatable :: filters(:)
    integer :: k

    status = status_type()
    k = ibuffer_local_index(.true.)
    visible = buf_matches_regexps(buf, locals(k)%tmp_show)
    if (visible) return
    if (buf_matches_regexps(buf, locals(k)%tmp_hide)) return
    if (buf_matches_predicates(buf, ibuffer_never_show_predicates)) return

    if (.not. all) then
        if (buf_matches_predicates(buf, ibuffer_maybe_show_predicates)) return
    end if

    if (.not. ibuffer_view_ibuffer) then
        if (ibuffer_buf%index == 0) return
        if (ibuffer_buf%index == buf%index) return
    end if

    filters = locals(k)%qualifiers
    visible = is_ibuffer_included_in_filters(buf, filters, status)
    if (.not. status%ok()) return
    if (.not. visible) visible = buf_matches_predicates(buf, ibuffer_always_show_predicates)
    end procedure is_ibuffer_ext_visible

    module procedure ibuffer_generate_filter_groups
    type(ibuffer_filter_groups_type) :: groups
    integer, allocatable :: of_group(:), counts(:), slot(:)
    logical :: no_empty, no_default, hit
    integer :: k, g, n, m

    status = status_type()
    no_empty = .false.
    if (present(noempty)) no_empty = noempty
    no_default = .false.
    if (present(nodefault)) no_default = nodefault
    k = ibuffer_local_index(.true.)
    groups = locals(k)%filter_groups
    if (.not. allocated(groups%groups)) allocate (groups%groups(0))
    if (.not. no_default) call groups%add("Default")
    n = groups%count()
    allocate (of_group(size(bmarklist)), counts(n), slot(n), group_of(size(bmarklist)))
    of_group = 0
    counts = 0

    do g = 1, n
        do k = 1, size(bmarklist)
            if (of_group(k) /= 0) cycle
            hit = is_ibuffer_included_in_filters(bmarklist(k)%buffer, groups%groups(g)%filters, status)
            if (.not. status%ok()) return

            if (hit) then
                of_group(k) = g
                counts(g) = counts(g) + 1
            end if
        end do
    end do

    ! (push (cons NAME BUFS) ret): the last group first.
    allocate (names(n))
    m = 0
    slot = 0

    do g = n, 1, -1
        if (no_empty .and. counts(g) == 0) cycle
        m = m + 1
        names(m) = groups%groups(g)%name
        slot(g) = m
    end do

    names = names(1:m)

    do k = 1, size(bmarklist)
        group_of(k) = 0
        if (of_group(k) > 0) group_of(k) = slot(of_group(k))
    end do
    end procedure ibuffer_generate_filter_groups

    ! ------------------------------------------------------------------
    ! Filters
    ! ------------------------------------------------------------------

    module procedure is_ibuffer_included_in_filters
    type(status_type) :: st
    logical :: one
    integer :: j

    status = status_type()
    included = .true.

    ! (A filter will return false if it failed; all are tried.)
    do j = 1, size(filters)
        one = is_ibuffer_included_in_filter(buf, filters(j), st)

        if (.not. st%ok()) then
            status = st
            included = .false.
            return
        end if

        if (.not. one) included = .false.
    end do
    end procedure is_ibuffer_included_in_filters

    module procedure ibuffer_unary_operand
    integer :: n

    if (.not. allocated(filter%nodes)) then
        allocate (operand%nodes(0))
    else if (size(filter%nodes) < 2) then
        allocate (operand%nodes(0))
    else
        n = ibuffer_filter_extent(filter, 2)
        operand = ibuffer_filter_part(filter, 2, n)
    end if
    end procedure ibuffer_unary_operand

    module procedure is_ibuffer_included_in_filter
    type(ibuffer_filter_type) :: inner

    status = status_type()
    included = .false.
    if (.not. allocated(filter%nodes)) return
    if (size(filter%nodes) == 0) return

    if (filter%nodes(1)%kind == IBUFFER_FILTER_NOT) then
        inner = ibuffer_unary_operand(filter)
        if (size(inner%nodes) == 0) return

        ! Allows (not (not ...)) etc, which may be overkill
        if (inner%nodes(1)%kind == IBUFFER_FILTER_NOT) then
            included = is_ibuffer_included_in_filter(buf, ibuffer_unary_operand(inner), status)
        else
            included = .not. included_in_filter_p_1(buf, inner, status)
            if (.not. status%ok()) included = .false.
        end if
    else
        included = included_in_filter_p_1(buf, filter, status)
    end if
    end procedure is_ibuffer_included_in_filter

    ! Port of ibuf-ext.el:ibuffer-included-in-filter-p-1.
    recursive logical function included_in_filter_p_1(buf, filter, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_type), intent(in) :: filter
        type(status_type), intent(out) :: status
        type(ibuffer_filter_type), allocatable :: parts(:), saved(:)
        type(status_type) :: st
        logical :: one
        integer :: j, i

        status = status_type()
        included = .false.

        select case (filter%nodes(1)%kind)
        case (IBUFFER_FILTER_OR)
            call operands(filter, parts)

            do j = 1, size(parts)
                one = is_ibuffer_included_in_filter(buf, parts(j), status)

                if (.not. status%ok()) then
                    included = .false.
                    return
                end if

                if (one) included = .true.
            end do
        case (IBUFFER_FILTER_AND)
            call operands(filter, parts)
            included = .true.

            do j = 1, size(parts)
                if (.not. is_ibuffer_included_in_filter(buf, parts(j), status)) then
                    included = .false.
                    return
                end if

                if (.not. status%ok()) return
            end do
        case (IBUFFER_FILTER_SAVED)
            i = ibuffer_saved_filters%find(qualifier_text(filter%nodes(1)))

            if (i == 0) then
                call ibuffer_filter_disable(st, .true.)
                call set_error(status, FM_GENERIC_ERROR, "Unknown saved filter "//qualifier_text(filter%nodes(1)))
                return
            end if

            saved = ibuffer_saved_filters%entries(i)%filters
            included = is_ibuffer_included_in_filters(buf, saved, status)
        case (IBUFFER_FILTER_NOT)
            ! (Only reached through `is_ibuffer_included_in_filter'.)
            included = is_ibuffer_included_in_filter(buf, filter, status)
        case default
            i = filter_index(filter%nodes(1)%name)

            if (i == 0) then
                call ibuffer_filter_disable(st, .true.)
                call set_error(status, FM_GENERIC_ERROR, "Undefined filter "//trim(filter%nodes(1)%name))
                return
            end if

            included = ibuffer_filtering_table(i)%test(buf, filter%nodes(1), st)

            if (.not. st%ok()) then
                ! (condition-case: an error pops the filter.)
                included = .false.
                call ibuffer_pop_filter(status)
            end if
        end select
    end function included_in_filter_p_1

    ! The index in `ibuffer_filtering_table' of the filter NAME (0 for
    ! none).  Not a port.
    integer function filter_index(name) result(i)
        character(len=*), intent(in) :: name

        if (allocated(ibuffer_filtering_table)) then
            do i = 1, size(ibuffer_filtering_table)
                if (ibuffer_filtering_table(i)%name == name) return
            end do
        end if

        i = 0
    end function filter_index

    ! The string qualifier of NODE.  Not a port.
    function qualifier_text(node) result(s)
        type(ibuffer_filter_node_type), intent(in) :: node
        character(len=:), allocatable :: s

        s = node%qualifier(1:node%qualifier_len)
    end function qualifier_text

    ! The operands of the compound FILTER.  Not a port.
    subroutine operands(filter, parts)
        type(ibuffer_filter_type), intent(in) :: filter
        type(ibuffer_filter_type), allocatable, intent(out) :: parts(:)
        integer :: j, k, n

        allocate (parts(filter%nodes(1)%count))
        k = 2

        do j = 1, size(parts)
            n = ibuffer_filter_extent(filter, k)
            parts(j) = ibuffer_filter_part(filter, k, n)
            k = k + n
        end do
    end subroutine operands

    ! The compound filter of KIND (`not', `and' or `or') with the operands
    ! PARTS.  Not a port.
    function compound(kind, parts) result(f)
        integer, intent(in) :: kind
        type(ibuffer_filter_type), intent(in) :: parts(:)
        type(ibuffer_filter_type) :: f
        integer :: j, n, at

        n = 1

        do j = 1, size(parts)
            n = n + size(parts(j)%nodes)
        end do

        allocate (f%nodes(n))
        f%nodes(1)%kind = kind
        f%nodes(1)%count = size(parts)
        at = 2

        do j = 1, size(parts)
            f%nodes(at:at + size(parts(j)%nodes) - 1) = parts(j)%nodes
            at = at + size(parts(j)%nodes)
        end do
    end function compound

    ! Add F before the filters of LIST.  Not a port.
    subroutine add_front(list, f)
        type(ibuffer_filter_type), allocatable, intent(inout) :: list(:)
        type(ibuffer_filter_type), intent(in) :: f
        type(ibuffer_filter_type), allocatable :: grown(:)
        integer :: j

        allocate (grown(size(list) + 1))
        grown(1) = f

        do j = 1, size(list)
            grown(j + 1) = list(j)
        end do

        call move_alloc(grown, list)
    end subroutine add_front

    ! The filters A followed by B.  Not a port.
    function joined(a, b) result(ab)
        type(ibuffer_filter_type), intent(in) :: a(:), b(:)
        type(ibuffer_filter_type), allocatable :: ab(:)
        integer :: j

        allocate (ab(size(a) + size(b)))

        do j = 1, size(a)
            ab(j) = a(j)
        end do

        do j = 1, size(b)
            ab(size(a) + j) = b(j)
        end do
    end function joined

    module procedure ibuffer_format_qualifier
    if (.not. allocated(qualifier%nodes)) then
        s = ""
    else if (size(qualifier%nodes) == 0) then
        s = ""
    else if (qualifier%nodes(1)%kind == IBUFFER_FILTER_NOT) then
        s = " [NOT"//format_qualifier_1(ibuffer_unary_operand(qualifier))//"]"
    else
        s = format_qualifier_1(qualifier)
    end if
    end procedure ibuffer_format_qualifier

    ! Port of ibuf-ext.el:ibuffer-format-qualifier-1.
    recursive function format_qualifier_1(qualifier) result(s)
        type(ibuffer_filter_type), intent(in) :: qualifier
        character(len=:), allocatable :: s
        type(ibuffer_filter_type), allocatable :: parts(:)
        character(len=32) :: buf
        integer :: j, i

        if (.not. allocated(qualifier%nodes)) then
            s = ""
            return
        end if

        if (size(qualifier%nodes) == 0) then
            s = ""
            return
        end if

        select case (qualifier%nodes(1)%kind)
        case (IBUFFER_FILTER_SAVED)
            s = " [filter: "//qualifier_text(qualifier%nodes(1))//"]"
        case (IBUFFER_FILTER_OR, IBUFFER_FILTER_AND)
            call operands(qualifier, parts)

            if (qualifier%nodes(1)%kind == IBUFFER_FILTER_OR) then
                s = " [OR"
            else
                s = " [AND"
            end if

            do j = 1, size(parts)
                s = s//ibuffer_format_qualifier(parts(j))
            end do

            s = s//"]"
        case (IBUFFER_FILTER_NOT)
            s = ibuffer_format_qualifier(qualifier)
        case default
            i = filter_index(qualifier%nodes(1)%name)
            s = " ["
            if (i > 0) s = s//trim(ibuffer_filtering_table(i)%description)

            associate (node => qualifier%nodes(1))
                ! (A predicate, a procedure, has no printed form.)
                if (node%has_qualifier .and. .not. associated(node%predicate)) then
                    if (node%is_number) then
                        write (buf, "(i0)") node%number
                        s = s//": "//trim(buf)
                    else
                        s = s//": "//qualifier_text(node)
                    end if
                end if
            end associate

            s = s//"]"
        end select
    end function format_qualifier_1

    module procedure ibuffer_push_filter
    integer :: k, j
    type(ibuffer_filter_type), allocatable :: filters(:)

    k = ibuffer_local_index(.true.)
    pushed = .false.

    do j = 1, size(locals(k)%qualifiers)
        if (ibuffer_filter_equal(locals(k)%qualifiers(j), filter_specification)) return
    end do

    filters = locals(k)%qualifiers
    call add_front(filters, filter_specification)
    call ibuffer_set_filtering_qualifiers(filters)
    pushed = .true.
    end procedure ibuffer_push_filter

    !> Toggle use of Ibuffer's auto-update facility (Ibuffer Auto mode).
    !> Port of ibuf-ext.el:ibuffer-auto-mode.
    recursive subroutine ibuffer_auto_mode(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        logical :: changed

        status = status_type()

        if (.not. is_derived_mode("ibuffer_mode")) then
            call set_error(status, FM_GENERIC_ERROR, "This buffer is not in Ibuffer mode")
            return
        end if

        if (enable) then
            ! Initialize state vector
            changed = is_frame_or_buffer_changed(auto_buffers_changed)
            call post_command_hook%add(ibuffer_auto_update_changed)
        else
            call post_command_hook%remove(ibuffer_auto_update_changed)
        end if

        call update_mode_line_process()
    end subroutine ibuffer_auto_mode

    recursive subroutine cmd_ibuffer_auto_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%ibuffer_auto_mode%command(status)
    end subroutine cmd_ibuffer_auto_mode

    ! Port of ibuf-ext.el:ibuffer-auto-update-changed.
    recursive subroutine ibuffer_auto_update_changed(status)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: bufs(:)
        type(handle_type) :: old
        type(status_type) :: st
        integer :: k

        status = status_type()
        if (.not. is_frame_or_buffer_changed(auto_buffers_changed)) return
        old = current_buffer()
        bufs = buffer_list()

        do k = 1, size(bufs)
            if (.not. is_buffer_live(bufs(k))) cycle
            call set_buffer(bufs(k), st)

            if (minor_modes%ibuffer_auto_mode%enabled() .and. is_derived_mode("ibuffer_mode")) then
                call ibuffer_update(st, .false., .true.)
            end if
        end do

        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine ibuffer_auto_update_changed

    !> Toggle the display status of the filter group on this line.
    !> Port of ibuf-ext.el:ibuffer-toggle-filter-group.
    recursive subroutine ibuffer_toggle_filter_group(status)
        type(status_type), intent(out) :: status

        call ibuffer_toggle_filter_group_1(point(), status)
    end subroutine ibuffer_toggle_filter_group

    ! Port of ibuf-ext.el:ibuffer-toggle-filter-group-1.
    recursive subroutine ibuffer_toggle_filter_group_1(posn, status)
        integer(int64), intent(in) :: posn
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        character(len=256), allocatable :: grown(:)
        integer :: k, j, n

        status = status_type()

        if (.not. has_string_prop(posn, "ibuffer_filter_group_name")) then
            call set_error(status, FM_GENERIC_ERROR, "No filtering group name present")
            return
        end if

        name = string_prop(posn, "ibuffer_filter_group_name")
        k = ibuffer_local_index(.true.)

        if (any(locals(k)%hidden_groups == name)) then
            n = 0
            allocate (grown(size(locals(k)%hidden_groups)))

            do j = 1, size(locals(k)%hidden_groups)
                if (locals(k)%hidden_groups(j) == name) cycle
                n = n + 1
                grown(n) = locals(k)%hidden_groups(j)
            end do

            locals(k)%hidden_groups = grown(1:n)
        else
            n = size(locals(k)%hidden_groups)
            allocate (grown(n + 1))
            grown(1) = name
            grown(2:n + 1) = locals(k)%hidden_groups
            call move_alloc(grown, locals(k)%hidden_groups)
        end if

        call ibuffer_update(status, .false., .true.)
    end subroutine ibuffer_toggle_filter_group_1

    !> Move point forwards by COUNT filtering groups.  Port of
    !> ibuf-ext.el:ibuffer-forward-filter-group.
    recursive subroutine ibuffer_forward_filter_group(count)
        integer(int64), intent(in), optional :: count
        integer(int64) :: n, next
        logical :: found
        type(status_type) :: st

        n = 1
        if (present(count)) n = count

        do while (n > 0)
            if (has_prop(point(), "ibuffer_filter_group_name")) then
                call next_single_property_change(point(), value_type(name="ibuffer_filter_group_name"), next, found, st, &
                                                 point_max())
                if (.not. found) next = point_max()
                call goto_char(next)
            end if

            call next_single_property_change(point(), value_type(name="ibuffer_filter_group_name"), next, found, st, point_max())
            if (.not. found) next = point_max()
            call goto_char(next)
            n = n - 1
        end do

        call ibuffer_forward_line(0_int64)
    end subroutine ibuffer_forward_filter_group

    recursive subroutine cmd_ibuffer_forward_filter_group(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. allocated(current_prefix_arg%a)) then
            call ibuffer_forward_filter_group()
        else
            call ibuffer_forward_filter_group(prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_ibuffer_forward_filter_group

    !> Move point backwards by COUNT filtering groups.  Port of
    !> ibuf-ext.el:ibuffer-backward-filter-group.
    recursive subroutine ibuffer_backward_filter_group(count)
        integer(int64), intent(in), optional :: count
        integer(int64) :: n, prev
        logical :: found
        type(status_type) :: st

        n = 1
        if (present(count)) n = count

        do while (n > 0)
            if (has_prop(point(), "ibuffer_filter_group_name")) then
                call previous_single_property_change(point(), value_type(name="ibuffer_filter_group_name"), prev, found, st, &
                                                     point_min())
                if (.not. found) prev = point_min()
                call goto_char(prev)
            end if

            call previous_single_property_change(point(), value_type(name="ibuffer_filter_group_name"), prev, found, st, &
                                                 point_min())
            if (.not. found) prev = point_min()
            call goto_char(prev)
            n = n - 1
        end do

        if (point() == point_min()) then
            call goto_char(point_max())
            call ibuffer_backward_filter_group(1_int64)
        end if

        call ibuffer_forward_line(0_int64)
    end subroutine ibuffer_backward_filter_group

    recursive subroutine cmd_ibuffer_backward_filter_group(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. allocated(current_prefix_arg%a)) then
            call ibuffer_backward_filter_group()
        else
            call ibuffer_backward_filter_group(prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_ibuffer_backward_filter_group

    ! ibuffer-current-filter-groups-with-position: the names and positions
    ! of the filter group headers.
    ! Port of ibuf-ext.el:ibuffer-current-filter-groups-with-position.
    subroutine current_filter_groups_with_position(names, positions, n)
        character(len=256), allocatable, intent(out) :: names(:)
        integer(int64), allocatable, intent(out) :: positions(:)
        integer, intent(out) :: n
        character(len=256), allocatable :: gn(:)
        integer(int64), allocatable :: gp(:)
        integer(int64) :: saved, pos, next
        logical :: found
        type(status_type) :: st

        saved = point()
        allocate (names(8), positions(8))
        n = 0
        call goto_char(point_min())

        do while (.not. is_eob())
            call next_single_property_change(point(), value_type(name="ibuffer_filter_group_name"), pos, found, st)
            if (.not. found) exit
            call goto_char(pos)

            if (n == size(names)) then
                allocate (gn(2*n), gp(2*n))
                gn(1:n) = names
                gp(1:n) = positions
                call move_alloc(gn, names)
                call move_alloc(gp, positions)
            end if

            n = n + 1
            names(n) = string_prop(point(), "ibuffer_filter_group_name")
            positions(n) = pos
            call next_single_property_change(pos, value_type(name="ibuffer_filter_group_name"), next, found, st)
            if (.not. found) exit
            call goto_char(next)
        end do

        call goto_char(saved)
    end subroutine current_filter_groups_with_position

    !> Move point to the filter group whose name is NAME.  Port of
    !> ibuf-ext.el:ibuffer-jump-to-filter-group.
    recursive subroutine ibuffer_jump_to_filter_group(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        character(len=256), allocatable :: names(:)
        integer(int64), allocatable :: positions(:)
        integer :: n, k

        status = status_type()
        call current_filter_groups_with_position(names, positions, n)

        do k = 1, n
            if (names(k) == name) then
                call goto_char(positions(k))
                return
            end if
        end do

        call set_error(status, FM_GENERIC_ERROR, "No filter group with name "//name)
    end subroutine ibuffer_jump_to_filter_group

    recursive subroutine cmd_ibuffer_jump_to_filter_group(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_filter_group_name("Jump to filter group: ", name, status)
        if (status%ok()) call ibuffer_jump_to_filter_group(name, status)
    end subroutine cmd_ibuffer_jump_to_filter_group

    ! ------------------------------------------------------------------
    ! Operations
    ! ------------------------------------------------------------------

    ! ibuffer--maybe-erase-shell-cmd-output.
    ! Port of ibuf-ext.el:ibuffer--maybe-erase-shell-cmd-output.
    recursive subroutine maybe_erase_shell_cmd_output(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, old
        type(status_type) :: st

        status = status_type()
        buf = get_buffer(shell_command_buffer_name)
        if (.not. is_buffer_live(buf)) return
        if (shell_command_dont_erase_buffer%value /= 0) return
        old = current_buffer()
        call set_buffer(buf, st)
        if (buffer_size() /= 0) call erase_buffer(st)
        call set_buffer(old, st)
    end subroutine maybe_erase_shell_cmd_output

    !> Pipe the contents of each marked buffer to shell command COMMAND.
    !> Port of ibuf-ext.el:ibuffer-do-shell-command-pipe.
    recursive subroutine ibuffer_do_shell_command_pipe(command, status)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status
        type(ibuffer_op_type) :: op

        op_command = command
        op%opstring = "Shell command executed on"
        op%before => maybe_erase_shell_cmd_output
        op%modifier_p = IBUFFER_MODIFIER_NEVER
        op%body => shell_command_pipe_body
        call ibuffer_operate(op, status)
    end subroutine ibuffer_do_shell_command_pipe

    recursive subroutine cmd_ibuffer_do_shell_command_pipe(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: command

        call read_string_arg("Pipe to shell command: ", command, status)
        if (status%ok()) call ibuffer_do_shell_command_pipe(command, status)
    end subroutine cmd_ibuffer_do_shell_command_pipe

    ! Port of part of ibuf-ext.el:ibuffer-do-shell-command-pipe (its body).
    recursive subroutine shell_command_pipe_body(buf, mark, result, status)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        integer, intent(out) :: result
        type(status_type), intent(out) :: status
        type(handle_type) :: out_buf, here
        type(value_type) :: exit_value
        type(status_type) :: st

        if (mark == achar(0) .or. buf%index < 0) continue
        result = IBUFFER_OP_DONE
        here = current_buffer()
        call get_buffer_create(shell_command_buffer_name, out_buf, status)
        if (.not. status%ok()) return
        call set_buffer(out_buf, st)
        call goto_char(point_max())
        call set_buffer(here, st)
        call execute_shell_region(point_min(), point_max(), op_command, .false., exit_value, status, "", out_buf)
    end subroutine shell_command_pipe_body

    !> Replace the contents of marked buffers with output of pipe to
    !> COMMAND.  Port of ibuf-ext.el:ibuffer-do-shell-command-pipe-replace.
    recursive subroutine ibuffer_do_shell_command_pipe_replace(command, status)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status
        type(ibuffer_op_type) :: op

        op_command = command
        op%opstring = "Buffer contents replaced in"
        op%active_opstring = "replace buffer contents in"
        op%dangerous = .true.
        op%modifier_p = IBUFFER_MODIFIER_ALWAYS
        op%body => shell_command_pipe_replace_body
        call ibuffer_operate(op, status)
    end subroutine ibuffer_do_shell_command_pipe_replace

    recursive subroutine cmd_ibuffer_do_shell_command_pipe_replace(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: command

        call read_string_arg("Pipe to shell command (replace): ", command, status)
        if (status%ok()) call ibuffer_do_shell_command_pipe_replace(command, status)
    end subroutine cmd_ibuffer_do_shell_command_pipe_replace

    ! Port of part of ibuf-ext.el:ibuffer-do-shell-command-pipe-replace (its
    ! body).
    recursive subroutine shell_command_pipe_replace_body(buf, mark, result, status)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        integer, intent(out) :: result
        type(status_type), intent(out) :: status
        type(value_type) :: exit_value

        if (mark == achar(0)) continue
        result = IBUFFER_OP_DONE
        call execute_shell_region(point_min(), point_max(), op_command, .true., exit_value, status, "", buf)
    end subroutine shell_command_pipe_replace_body

    !> Run shell command COMMAND separately on files of marked buffers.
    !> Port of ibuf-ext.el:ibuffer-do-shell-command-file.
    recursive subroutine ibuffer_do_shell_command_file(command, status)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status
        type(ibuffer_op_type) :: op

        op_command = command
        op%opstring = "Shell command executed on"
        op%before => maybe_erase_shell_cmd_output
        op%modifier_p = IBUFFER_MODIFIER_NEVER
        op%body => shell_command_file_body
        call ibuffer_operate(op, status)
    end subroutine ibuffer_do_shell_command_file

    recursive subroutine cmd_ibuffer_do_shell_command_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: command

        call read_string_arg("Shell command on buffer's file: ", command, status)
        if (status%ok()) call ibuffer_do_shell_command_file(command, status)
    end subroutine cmd_ibuffer_do_shell_command_file

    ! Port of part of ibuf-ext.el:ibuffer-do-shell-command-file (its body).
    recursive subroutine shell_command_file_body(buf, mark, result, status)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        integer, intent(out) :: result
        type(status_type), intent(out) :: status
        type(handle_type) :: out_buf, here
        type(value_type) :: exit_value
        type(status_type) :: st
        character(len=:), allocatable :: file, msg
        logical :: modified

        if (mark == achar(0)) continue
        result = IBUFFER_OP_DONE
        file = ""
        call is_buffer_modified(buf, modified, st)
        if (.not. modified .and. allocated(buffers(cur)%filename)) file = buffers(cur)%filename
        call get_buffer_create(shell_command_buffer_name, out_buf, status)
        if (.not. status%ok()) return

        if (len(file) == 0 .or. .not. file_exists(file)) then
            call make_temp_file(buffers(cur)%name(1:min(10, len(buffers(cur)%name))), file, status)
            if (.not. status%ok()) return
            call write_region(file, .false., status, message=msg)
            if (.not. status%ok()) return
        end if

        here = current_buffer()
        call set_buffer(out_buf, st)
        call goto_char(point_max())
        call execute_process_shell_command(op_command//" "//shell_quote_argument(file), exit_value, status, &
                                           buffer=out_buf)
        call set_buffer(here, st)
    end subroutine shell_command_file_body

    !> Rename marked buffers as with `rename_uniquely'.  Port of
    !> ibuf-ext.el:ibuffer-do-rename-uniquely.
    recursive subroutine ibuffer_do_rename_uniquely(status)
        type(status_type), intent(out) :: status
        type(ibuffer_op_type) :: op

        op%opstring = "renamed"
        op%modifier_p = IBUFFER_MODIFIER_ALWAYS
        op%body => rename_uniquely_body
        call ibuffer_operate(op, status)
    end subroutine ibuffer_do_rename_uniquely

    ! Port of part of ibuf-ext.el:ibuffer-do-rename-uniquely (its body).
    recursive subroutine rename_uniquely_body(buf, mark, result, status)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        integer, intent(out) :: result
        type(status_type), intent(out) :: status

        if (mark == achar(0) .or. buf%index < 0) continue
        result = IBUFFER_OP_DONE
        status = status_type()
        call rename_uniquely()
    end subroutine rename_uniquely_body

    !> Revert marked buffers as with `revert_buffer'.  Port of
    !> ibuf-ext.el:ibuffer-do-revert.
    recursive subroutine ibuffer_do_revert(status)
        type(status_type), intent(out) :: status
        type(ibuffer_op_type) :: op

        op%dangerous = .true.
        op%opstring = "reverted"
        op%active_opstring = "revert"
        op%modifier_p = IBUFFER_MODIFIER_MAYBE
        op%body => revert_body
        call ibuffer_operate(op, status)
    end subroutine ibuffer_do_revert

    ! Port of part of ibuf-ext.el:ibuffer-do-revert (its body).
    recursive subroutine revert_body(buf, mark, result, status)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        integer, intent(out) :: result
        type(status_type), intent(out) :: status

        if (mark == achar(0) .or. buf%index < 0) continue
        result = IBUFFER_OP_DONE
        call revert_buffer(status, ignore_auto=.true., noconfirm=.true.)
    end subroutine revert_body

    !> Perform a `replace_regexp' in marked buffers.  Port of
    !> ibuf-ext.el:ibuffer-do-replace-regexp.
    recursive subroutine ibuffer_do_replace_regexp(from_str, to_str, status)
        character(len=*), intent(in) :: from_str, to_str
        type(status_type), intent(out) :: status
        type(ibuffer_op_type) :: op

        op_from = from_str
        op_to = to_str
        op%opstring = "replaced in"
        op%complex = .true.
        op%modifier_p = IBUFFER_MODIFIER_MAYBE
        op%body => replace_regexp_body
        call ibuffer_operate(op, status)
    end subroutine ibuffer_do_replace_regexp

    recursive subroutine cmd_ibuffer_do_replace_regexp(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: from_str, to_str

        call read_from_minibuffer("Replace regexp: ", from_str, status)
        if (.not. status%ok()) return
        call read_from_minibuffer("Replace "//from_str//" with: ", to_str, status)
        if (.not. status%ok()) return
        call ibuffer_do_replace_regexp(from_str, to_str, status)
    end subroutine cmd_ibuffer_do_replace_regexp

    ! Port of part of ibuf-ext.el:ibuffer-do-replace-regexp (its body).
    recursive subroutine replace_regexp_body(buf, mark, result, status)
        use search_fm, only: case_fold_search
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        integer, intent(out) :: result
        type(status_type), intent(out) :: status
        type(window_configuration_type) :: config
        type(status_type) :: st
        integer(int64) :: saved, found
        logical :: saved_fold

        if (mark == achar(0)) continue
        result = IBUFFER_OP_DONE
        call current_window_configuration(config)
        call switch_to_buffer(buffers(buf%index)%name, status)

        if (status%ok()) then
            saved = point()
            call goto_char(point_min())
            saved_fold = case_fold_search%value
            case_fold_search = ibuffer_case_fold_search

            do
                call re_search_forward(op_from, found, status, noerror=1)
                if (.not. status%ok() .or. found == 0) exit
                call replace_match(op_to, status)
                if (.not. status%ok()) exit
            end do

            case_fold_search = saved_fold
            call goto_char(min(saved, point_max()))
        end if

        call set_window_configuration(config)
        if (.not. st%ok()) continue
    end subroutine replace_regexp_body

    !> Perform a `query_replace' in marked buffers.  Port of
    !> ibuf-ext.el:ibuffer-do-query-replace.
    recursive subroutine ibuffer_do_query_replace(from_string, to_string, status, delimited, backward)
        character(len=*), intent(in) :: from_string, to_string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delimited, backward

        call query_replace_op(from_string, to_string, .false., status, delimited, backward)
    end subroutine ibuffer_do_query_replace

    recursive subroutine cmd_ibuffer_do_query_replace(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: from, to
        logical :: delimited, backward, counted

        call query_replace_read_args("Query replace", .false., from, to, delimited, backward, counted, status)
        if (status%ok()) call ibuffer_do_query_replace(from, to, status, delimited, backward)
    end subroutine cmd_ibuffer_do_query_replace

    !> Perform a `query_replace_regexp' in marked buffers.  Port of
    !> ibuf-ext.el:ibuffer-do-query-replace-regexp.
    recursive subroutine ibuffer_do_query_replace_regexp(regexp, to_string, status, delimited, backward)
        character(len=*), intent(in) :: regexp, to_string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delimited, backward

        call query_replace_op(regexp, to_string, .true., status, delimited, backward)
    end subroutine ibuffer_do_query_replace_regexp

    recursive subroutine cmd_ibuffer_do_query_replace_regexp(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: from, to
        logical :: delimited, backward, counted

        call query_replace_read_args("Query replace regexp", .true., from, to, delimited, backward, counted, status)
        if (status%ok()) call ibuffer_do_query_replace_regexp(from, to, status, delimited, backward)
    end subroutine cmd_ibuffer_do_query_replace_regexp

    ! Port of part of ibuf-ext.el:ibuffer-do-query-replace and
    ! ibuf-ext.el:ibuffer-do-query-replace-regexp.
    recursive subroutine query_replace_op(from, to, regexp, status, delimited, backward)
        character(len=*), intent(in) :: from, to
        logical, intent(in) :: regexp
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delimited, backward
        type(ibuffer_op_type) :: op

        op_from = from
        op_to = to
        op_regexp = regexp
        op_delimited = .false.
        if (present(delimited)) op_delimited = delimited
        op_backward = .false.
        if (present(backward)) op_backward = backward
        op%opstring = "replaced in"
        op%complex = .true.
        op%modifier_p = IBUFFER_MODIFIER_MAYBE
        op%body => query_replace_body
        call ibuffer_operate(op, status)
    end subroutine query_replace_op

    ! Port of part of ibuf-ext.el:ibuffer-do-query-replace and
    ! ibuf-ext.el:ibuffer-do-query-replace-regexp (their body).
    recursive subroutine query_replace_body(buf, mark, result, status)
        use search_fm, only: case_fold_search
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        integer, intent(out) :: result
        type(status_type), intent(out) :: status
        type(window_configuration_type) :: config
        integer(int64) :: saved
        logical :: saved_fold

        if (mark == achar(0)) continue
        result = IBUFFER_OP_DONE
        call current_window_configuration(config)
        call switch_to_buffer(buffers(buf%index)%name, status)

        if (status%ok()) then
            saved = point()
            saved_fold = case_fold_search%value
            case_fold_search = ibuffer_case_fold_search
            call goto_char(point_min())

            if (op_regexp) then
                call query_replace_regexp(op_from, op_to, status, op_delimited, backward=op_backward)
            else
                call query_replace(op_from, op_to, status, op_delimited, backward=op_backward)
            end if

            case_fold_search = saved_fold
            call goto_char(min(saved, point_max()))
        end if

        call set_window_configuration(config)
    end subroutine query_replace_body

    ! ------------------------------------------------------------------
    ! Sorting
    ! ------------------------------------------------------------------

    !> Toggle the current sorting mode.
    !> Default sorting modes are:
    !>  Recency - the last time the buffer was viewed
    !>  Name - the name of the buffer
    !>  Major Mode - the name of the major mode of the buffer
    !>  Size - the size of the buffer
    !> Port of ibuf-ext.el:ibuffer-toggle-sorting-mode.
    recursive subroutine ibuffer_toggle_sorting_mode(status)
        type(status_type), intent(out) :: status
        character(len=IBUFFER_NAME_LEN), allocatable :: modes(:)
        character(len=IBUFFER_NAME_LEN) :: tmp
        integer :: k, j, n, l

        n = size(ibuffer_sorting_functions)
        allocate (modes(n + 1))

        do k = 1, n
            modes(k) = ibuffer_sorting_functions(k)%name
        end do

        if (.not. any(modes(1:n) == "recency")) then
            n = n + 1
            modes(n) = "recency"
        end if

        ! (sort modes #'string-lessp)
        do k = 2, n
            tmp = modes(k)
            j = k - 1

            do while (j >= 1)
                if (.not. llt(tmp, modes(j))) exit
                modes(j + 1) = modes(j)
                j = j - 1
            end do

            modes(j + 1) = tmp
        end do

        l = ibuffer_local_index(.true.)
        k = 0

        do j = 1, n
            if (modes(j) == locals(l)%sorting_mode .and. len(locals(l)%sorting_mode) > 0) k = j
        end do

        if (k > 0 .and. k < n) then
            locals(l)%sorting_mode = trim(modes(k + 1))
        else
            locals(l)%sorting_mode = trim(modes(1))
        end if

        call message("Sorting by "//locals(l)%sorting_mode)
        call ibuffer_redisplay(status, .true.)
    end subroutine ibuffer_toggle_sorting_mode

    !> Toggle whether or not sorting is in reverse order.  Port of
    !> ibuf-ext.el:ibuffer-invert-sorting.
    recursive subroutine ibuffer_invert_sorting(status)
        type(status_type), intent(out) :: status
        integer :: k

        k = ibuffer_local_index(.true.)
        locals(k)%sorting_reversep = .not. locals(k)%sorting_reversep

        if (locals(k)%sorting_reversep) then
            call message("Sorting order reversed")
        else
            call message("Sorting order normal")
        end if

        call ibuffer_redisplay(status, .true.)
    end subroutine ibuffer_invert_sorting

    ! The major mode name of BUF, downcased.  Not a port.
    function major_mode_of(buf) result(name)
        type(handle_type), intent(in) :: buf
        character(len=:), allocatable :: name

        name = "fundamental_mode"
        if (allocated(buffers(buf%index)%major_mode)) name = buffers(buf%index)%major_mode
    end function major_mode_of

    ! Not a port.
    function downcased(s) result(d)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: d
        integer :: k

        d = s

        do k = 1, len(d)
            if (d(k:k) >= "A" .and. d(k:k) <= "Z") d(k:k) = achar(iachar(d(k:k)) + 32)
        end do
    end function downcased

    ! (string-lessp A B)  Not a port.
    logical function is_string_less(a, b)
        character(len=*), intent(in) :: a, b
        integer :: k, n

        n = min(len(a), len(b))

        do k = 1, n
            if (a(k:k) /= b(k:k)) then
                is_string_less = iachar(a(k:k)) < iachar(b(k:k))
                return
            end if
        end do

        is_string_less = len(a) < len(b)
    end function is_string_less

    !> Sort the buffers by major modes.
    !> Ordering is lexicographic.  Port of ibuf-ext.el
    !> define-ibuffer-sorter major-mode.
    recursive logical function sort_major_mode(a, b) result(less)
        type(handle_type), intent(in) :: a, b

        less = is_string_less(downcased(major_mode_of(a)), downcased(major_mode_of(b)))
    end function sort_major_mode

    !> Sort the buffers by their mode name.
    !> Ordering is lexicographic.  Port of ibuf-ext.el
    !> define-ibuffer-sorter mode-name.
    recursive logical function sort_mode_name(a, b) result(less)
        type(handle_type), intent(in) :: a, b

        less = is_string_less(downcased(mode_name_of(a)), downcased(mode_name_of(b)))
    end function sort_mode_name

    !> Sort the buffers by their names.
    !> Ordering is lexicographic.  Port of ibuf-ext.el
    !> define-ibuffer-sorter alphabetic.
    recursive logical function sort_alphabetic(a, b) result(less)
        type(handle_type), intent(in) :: a, b

        less = is_string_less(buffers(a%index)%name, buffers(b%index)%name)
    end function sort_alphabetic

    !> Sort the buffers by their size.  Port of ibuf-ext.el
    !> define-ibuffer-sorter size.
    recursive logical function sort_size(a, b) result(less)
        type(handle_type), intent(in) :: a, b

        less = buffers(a%index)%text%z < buffers(b%index)%text%z
    end function sort_size

    ! The file name or process name of BUF ("" for none).  Not a port.
    function file_or_process(buf) result(name)
        type(handle_type), intent(in) :: buf
        character(len=:), allocatable :: name
        type(handle_type) :: old, proc
        type(status_type) :: st

        old = current_buffer()
        call set_buffer(buf, st)
        name = ibuffer_buffer_file_name()
        call set_buffer(old, st)
        if (len(name) > 0) return
        proc = get_buffer_process(buf)
        if (proc%index /= 0) name = process_name(proc)
    end function file_or_process

    !> Sort the buffers by their file name/process name.  Port of
    !> ibuf-ext.el define-ibuffer-sorter filename/process.
    recursive logical function sort_filename_process(a, b) result(less)
        type(handle_type), intent(in) :: a, b

        less = is_string_less(file_or_process(a), file_or_process(b))
    end function sort_filename_process

    !> Sort the buffers by major modes.
    !> Ordering is lexicographic.  Port of
    !> ibuf-ext.el:ibuffer-do-sort-by-major-mode.
    recursive subroutine ibuffer_do_sort_by_major_mode(status)
        type(status_type), intent(out) :: status

        call ibuffer_do_sort_by("major_mode", status)
    end subroutine ibuffer_do_sort_by_major_mode

    !> Sort the buffers by their mode name.
    !> Ordering is lexicographic.  Port of
    !> ibuf-ext.el:ibuffer-do-sort-by-mode-name.
    recursive subroutine ibuffer_do_sort_by_mode_name(status)
        type(status_type), intent(out) :: status

        call ibuffer_do_sort_by("mode_name", status)
    end subroutine ibuffer_do_sort_by_mode_name

    !> Sort the buffers by their names.
    !> Ordering is lexicographic.  Port of
    !> ibuf-ext.el:ibuffer-do-sort-by-alphabetic.
    recursive subroutine ibuffer_do_sort_by_alphabetic(status)
        type(status_type), intent(out) :: status

        call ibuffer_do_sort_by("alphabetic", status)
    end subroutine ibuffer_do_sort_by_alphabetic

    !> Sort the buffers by their size.  Port of
    !> ibuf-ext.el:ibuffer-do-sort-by-size.
    recursive subroutine ibuffer_do_sort_by_size(status)
        type(status_type), intent(out) :: status

        call ibuffer_do_sort_by("size", status)
    end subroutine ibuffer_do_sort_by_size

    !> Sort the buffers by their file name/process name.  Port of
    !> ibuf-ext.el:ibuffer-do-sort-by-filename/process.
    recursive subroutine ibuffer_do_sort_by_filename_process(status)
        type(status_type), intent(out) :: status

        call ibuffer_do_sort_by("filename/process", status)
    end subroutine ibuffer_do_sort_by_filename_process

    ! ------------------------------------------------------------------
    ! Handy functions
    ! ------------------------------------------------------------------

    ! Read a string after PROMPT (an "s" interactive argument).  Not a port.
    recursive subroutine read_string_arg(prompt, result, status, initial)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: initial

        call read_from_minibuffer(prompt, result, status, initial=initial)
    end subroutine read_string_arg

    !> Add REGEXP to `ibuffer_tmp_hide_regexps'.
    !> This means that buffers whose name matches REGEXP will not be shown
    !> for this Ibuffer session.  Port of ibuf-ext.el:ibuffer-add-to-tmp-hide.
    subroutine ibuffer_add_to_tmp_hide(regexp)
        character(len=*), intent(in) :: regexp
        character(len=256), allocatable :: grown(:)
        integer :: k, n

        k = ibuffer_local_index(.true.)
        n = size(locals(k)%tmp_hide)
        allocate (grown(n + 1))
        grown(1) = regexp
        grown(2:n + 1) = locals(k)%tmp_hide
        call move_alloc(grown, locals(k)%tmp_hide)
    end subroutine ibuffer_add_to_tmp_hide

    recursive subroutine cmd_ibuffer_add_to_tmp_hide(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        type(handle_type) :: buf

        buf = ibuffer_current_buffer(status, .true.)
        if (.not. status%ok()) return
        call read_string_arg("Never show buffers matching: ", regexp, status, regexp_quote(buffers(buf%index)%name))
        if (status%ok()) call ibuffer_add_to_tmp_hide(regexp)
    end subroutine cmd_ibuffer_add_to_tmp_hide

    !> Add REGEXP to `ibuffer_tmp_show_regexps'.
    !> This means that buffers whose name matches REGEXP will always be shown
    !> for this Ibuffer session.  Port of ibuf-ext.el:ibuffer-add-to-tmp-show.
    subroutine ibuffer_add_to_tmp_show(regexp)
        character(len=*), intent(in) :: regexp
        character(len=256), allocatable :: grown(:)
        integer :: k, n

        k = ibuffer_local_index(.true.)
        n = size(locals(k)%tmp_show)
        allocate (grown(n + 1))
        grown(1) = regexp
        grown(2:n + 1) = locals(k)%tmp_show
        call move_alloc(grown, locals(k)%tmp_show)
    end subroutine ibuffer_add_to_tmp_show

    recursive subroutine cmd_ibuffer_add_to_tmp_show(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        type(handle_type) :: buf

        buf = ibuffer_current_buffer(status, .true.)
        if (.not. status%ok()) return
        call read_string_arg("Always show buffers matching: ", regexp, status, regexp_quote(buffers(buf%index)%name))
        if (status%ok()) call ibuffer_add_to_tmp_show(regexp)
    end subroutine cmd_ibuffer_add_to_tmp_show

    !> Move forward by COUNT marked buffers (default 1).
    !>
    !> If MARK is present, it should be a character denoting the type of
    !> mark to move by.  The default is `ibuffer_marked_char'.
    !>
    !> If DIRECTION is present, it should be an integer; negative integers
    !> mean move backwards, non-negative integers mean move forwards.
    !> Port of ibuf-ext.el:ibuffer-forward-next-marked.
    recursive subroutine ibuffer_forward_next_marked(status, count, mark, direction)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        character, intent(in), optional :: mark
        integer(int64), intent(in), optional :: direction
        character :: m
        integer(int64) :: dir, opos

        status = status_type()
        if (present(count)) continue
        m = ibuffer_marked_char
        if (present(mark)) m = mark
        dir = 1
        if (present(direction)) dir = direction
        ! Skip the title
        call ibuffer_forward_line(0_int64)
        opos = point()
        call ibuffer_forward_line(dir)

        do while (.not. (point() == opos .or. ibuffer_current_mark() == m))
            call ibuffer_forward_line(dir)
        end do

        if (point() == opos .and. ibuffer_current_mark() /= m) then
            call set_error(status, FM_GENERIC_ERROR, "No buffers with mark "//m)
        end if
    end subroutine ibuffer_forward_next_marked

    recursive subroutine cmd_ibuffer_forward_next_marked(status)
        type(status_type), intent(out) :: status

        call ibuffer_forward_next_marked(status)
    end subroutine cmd_ibuffer_forward_next_marked

    !> Move backwards by COUNT marked buffers (default 1).
    !>
    !> If MARK is present, it should be a character denoting the type of
    !> mark to move by.  The default is `ibuffer_marked_char'.  Port of
    !> ibuf-ext.el:ibuffer-backwards-next-marked.
    recursive subroutine ibuffer_backwards_next_marked(status, count, mark)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        character, intent(in), optional :: mark

        call ibuffer_forward_next_marked(status, count, mark, -1_int64)
    end subroutine ibuffer_backwards_next_marked

    recursive subroutine cmd_ibuffer_backwards_next_marked(status)
        type(status_type), intent(out) :: status

        call ibuffer_backwards_next_marked(status)
    end subroutine cmd_ibuffer_backwards_next_marked

    !> Hide all of the currently marked lines.  Port of
    !> ibuf-ext.el:ibuffer-do-kill-lines.
    recursive subroutine ibuffer_do_kill_lines(status)
        type(status_type), intent(out) :: status
        integer :: count
        character(len=24) :: c

        if (ibuffer_count_marked_lines(status) == 0) then
            call message(substitute_command_keys("No buffers marked; use \<ibuffer_mode_map>" &
                                                 //"\[ibuffer_mark_forward] to mark a buffer"))
        else
            count = map_marked_lines(kill_line, status)
            if (.not. status%ok()) return
            write (c, "(i0)") count
            call message("Killed "//trim(c)//" lines")
        end if
    end subroutine ibuffer_do_kill_lines

    ! Port of part of ibuf-ext.el:ibuffer-do-kill-lines.
    recursive integer function kill_line(buf, mark) result(r)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark

        if (buf%index < 0 .or. mark == achar(0)) continue
        r = IBUFFER_OP_KILL
    end function kill_line

    !> Move point to the buffer whose name is NAME.
    !>
    !> If called interactively, prompt for a buffer name and go to the
    !> corresponding line in the Ibuffer buffer.  If said buffer is in a
    !> hidden group filter, open it.
    !>
    !> If `ibuffer_jump_offer_only_visible_buffers' is true, only offer
    !> visible buffers in the completion list.  Calling the command with
    !> a prefix argument reverses the meaning of that variable.  Port of
    !> ibuf-ext.el:ibuffer-jump-to-buffer.
    recursive subroutine ibuffer_jump_to_buffer(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        character(len=256), allocatable :: groups(:)
        integer :: k, j
        type(status_type) :: st

        status = status_type()
        if (len(name) == 0) return
        ! Blindly search for our buffer: it is very likely that it is not in
        ! a hidden filter group.
        call jump_to_buffer_by_name(name, st)
        if (st%ok()) return
        k = ibuffer_local_index(.true.)

        if (size(locals(k)%hidden_groups) > 0) then
            ! We did not find our buffer.  It must be in a hidden filter
            ! group, so go through all hidden filter groups to find it.
            groups = locals(k)%hidden_groups

            do j = 1, size(groups)
                call ibuffer_jump_to_filter_group(trim(groups(j)), status)
                if (.not. status%ok()) return
                call ibuffer_toggle_filter_group(status)
                if (.not. status%ok()) return
                call jump_to_buffer_by_name(name, st)
                if (st%ok()) return
                call ibuffer_jump_to_filter_group(trim(groups(j)), status)
                if (status%ok()) call ibuffer_toggle_filter_group(status)
                if (.not. status%ok()) return
            end do
        end if

        ! Still not found even though we expanded all hidden filter groups:
        ! that must be because it's hidden by predicate: we won't bother
        ! trying to display it.
        call set_error(status, FM_GENERIC_ERROR, "No buffer with name "//name)
    end subroutine ibuffer_jump_to_buffer

    recursive subroutine cmd_ibuffer_jump_to_buffer(status)
        type(status_type), intent(out) :: status
        type(ibuffer_state_type), allocatable :: states(:)
        type(value_type), allocatable :: names(:)
        type(completion_table_type) :: table
        character(len=:), allocatable :: name
        logical :: only_visible
        integer :: k

        only_visible = ibuffer_jump_offer_only_visible_buffers
        if (allocated(current_prefix_arg%a)) only_visible = .not. only_visible

        if (only_visible) then
            call ibuffer_current_state_list(states, status)
            if (.not. status%ok()) return

            if (size(states) == 0) then
                call set_error(status, FM_GENERIC_ERROR, "No buffers!")
                return
            end if

            allocate (names(size(states)))

            do k = 1, size(states)
                names(k) = value_type(text=buffers(states(k)%buffer%index)%name)
            end do

            table%kind = TABLE_LIST
            table%list = make_list(names)
            call completing_read("Jump to buffer: ", table, name, status, require_match=REQUIRE_MATCH_YES)
        else
            call read_buffer("Jump to buffer: ", name, status, require_match=REQUIRE_MATCH_YES)
        end if

        if (.not. status%ok()) return
        call ibuffer_jump_to_buffer(name, status)
    end subroutine cmd_ibuffer_jump_to_buffer

    ! Compare BUFFER with its associated file, if any.
    ! Unlike `diff_no_select', insert output into current buffer
    ! without erasing it.  Port of
    ! ibuf-ext.el:ibuffer-diff-buffer-with-file-1.
    recursive subroutine diff_buffer_with_file_1(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: old, newtmp, command, msg
        type(handle_type) :: here
        type(value_type) :: code
        type(status_type) :: st
        logical :: ro

        status = status_type()
        if (.not. allocated(buffers(buffer%index)%filename)) return
        if (len(buffers(buffer%index)%filename) == 0) return
        old = buffers(buffer%index)%filename
        ! (diff-file-local-copy of the buffer: a temporary file with its
        ! contents.)
        call make_temp_file("buffer-content-", newtmp, status)
        if (.not. status%ok()) return
        here = current_buffer()
        call set_buffer(buffer, st)
        call write_region(newtmp, .false., status, start=point_min(), end=point_max(), message=msg)
        call set_buffer(here, st)
        if (.not. status%ok()) return
        command = diff_command

        if (allocated(diff_switches)) then
            if (len(diff_switches) > 0) command = command//" "//diff_switches
        end if

        if (diff_use_labels == DIFF_LABELS_YES) then
            command = command//" --label "//shell_quote_argument(old)//" --label " &
                      //shell_quote_argument("#<buffer "//buffers(buffer%index)%name//">")
        end if

        command = command//" "//shell_quote_argument(old)//" "//shell_quote_argument(newtmp)
        ro = buffers(cur)%read_only
        buffers(cur)%read_only = .false.
        call insert(achar(10)//command//achar(10), status)
        call execute_process_shell_command(command, code, status)

        if (vtag(code) == V_INTEGER) then
            call diff_sentinel(int(xfixnum(code)), new_temp_file=newtmp)
        else
            call diff_sentinel(0, new_temp_file=newtmp)
        end if

        call goto_char(point_max())
        buffers(cur)%read_only = ro
    end subroutine diff_buffer_with_file_1

    !> View the differences between marked buffers and their associated
    !> files.  If no buffers are marked, use buffer at point.
    !> This requires the external program `diff_command' to be in your
    !> `exec_path'.  Port of ibuf-ext.el:ibuffer-diff-with-file.
    recursive subroutine ibuffer_diff_with_file(status)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: marked(:)
        type(handle_type) :: diff_buf, old
        type(status_type) :: st
        integer :: k

        call ibuffer_get_marked_buffers(marked, status)
        if (.not. status%ok()) return

        if (size(marked) == 0) then
            deallocate (marked)
            allocate (marked(1))
            marked(1) = ibuffer_current_buffer(status, .true.)
            if (.not. status%ok()) return
        end if

        call get_buffer_create("*Ibuffer Diff*", diff_buf, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(diff_buf, st)
        buffers(cur)%read_only = .true.
        buffers(cur)%undo_enabled = .false.
        buffers(cur)%read_only = .false.
        call erase_buffer(st)
        buffers(cur)%read_only = .true.
        buffers(cur)%undo_enabled = .true.
        call diff_mode(status)

        if (status%ok()) then
            call diff_check_labels()

            do k = 1, size(marked)
                if (.not. is_buffer_live(marked(k))) then
                    call set_error(status, FM_GENERIC_ERROR, "Buffer #<killed buffer> has been killed")
                    exit
                end if

                call diff_buffer_with_file_1(marked(k), status)
                if (.not. status%ok()) exit
            end do
        end if

        if (status%ok()) then
            call goto_char(point_min())

            if (following_char() == 10) then
                buffers(cur)%read_only = .false.
                call delete_region(point(), point() + 1, st)
                buffers(cur)%read_only = .true.
            end if
        end if

        call set_buffer(old, st)
        if (.not. status%ok()) return
        call pop_to_buffer_same_window(diff_buf, status)
    end subroutine ibuffer_diff_with_file

    ! ibuffer--near-buffers: the buffers of the N lines around point
    ! (after it if N is positive, before otherwise).
    ! Port of ibuffer.el:ibuffer--near-buffers.
    subroutine near_buffers(n, bufs)
        integer(int64), intent(in) :: n
        type(handle_type), allocatable, intent(out) :: bufs(:)
        integer(int64) :: x, saved, pos, shortage
        type(handle_type) :: b
        integer :: m

        allocate (bufs(abs(n)))
        m = 0
        saved = point()

        do x = 1, abs(n)
            call goto_char(saved)

            if (n >= 0) then
                shortage = forward_line(x - 1)
            else
                shortage = forward_line(-(x - 1) - 1)
            end if

            pos = pos_bol()
            b = serial_buffer(line_serial(pos))

            if (b%index /= 0) then
                m = m + 1
                bufs(m) = b
            end if
        end do

        call goto_char(saved)
        bufs = bufs(1:m)
    end subroutine near_buffers

    ! The buffers `ibuffer_copy_filename_as_kill' and
    ! `ibuffer_copy_buffername_as_kill' copy: next ARG buffers, or the
    ! marked buffers, or the one at point.  Not a port.
    recursive subroutine copy_buffers(bufs, status)
        type(handle_type), allocatable, intent(out) :: bufs(:)
        type(status_type), intent(out) :: status
        type(handle_type) :: b

        status = status_type()

        if (vtag(current_prefix_arg) == V_INTEGER) then
            if (xfixnum(current_prefix_arg) /= 0) then
                call near_buffers(xfixnum(current_prefix_arg), bufs)
                return
            end if
        end if

        call ibuffer_get_marked_buffers(bufs, status)

        if (size(bufs) == 0) then
            deallocate (bufs)
            b = ibuffer_current_buffer(status)

            if (b%index /= 0) then
                allocate (bufs(1))
                bufs(1) = b
            else
                allocate (bufs(0))
            end if
        end if
    end subroutine copy_buffers

    ! Copy STRING into the kill ring (appending after a kill-region).
    ! Not a port.
    recursive subroutine copy_as_kill(string)
        character(len=*), intent(in) :: string

        if (len(string) == 0) return

        if (value_eq(last_command, value_type(name="kill_region"))) then
            call kill_append(string, .false.)
        else
            call kill_new(string)
        end if

        call message(string)
    end subroutine copy_as_kill

    !> Copy filenames of marked (or next ARG) buffers into the kill ring.
    !>
    !> The names are separated by a space.
    !> If a buffer has no filename, it is ignored.
    !>
    !> With no prefix arg, use the filename sans its directory of each marked file.
    !> With a zero prefix arg, use the complete filename of each marked file.
    !> With \[universal_argument], use the filename of each marked file relative
    !> to `ibuffer_default_directory' if set, otherwise `default_directory'.
    !>
    !> You can then feed the file name(s) to other commands with \[yank].
    !> Port of ibuf-ext.el:ibuffer-copy-filename-as-kill.
    recursive subroutine ibuffer_copy_filename_as_kill(status)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: bufs(:)
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: string, name, dir
        integer :: k

        call copy_buffers(bufs, status)
        if (.not. status%ok()) return
        string = ""
        old = current_buffer()

        do k = 1, size(bufs)
            call set_buffer(bufs(k), st)
            name = ibuffer_buffer_file_name()
            dir = buffers(cur)%directory
            call set_buffer(old, st)
            if (len(name) == 0) cycle

            if (vtag(current_prefix_arg) == V_INTEGER) then
                if (xfixnum(current_prefix_arg) /= 0) name = file_name_nondirectory(name)
            else if (vtag(current_prefix_arg) == V_ARRAY) then
                if (allocated(ibuffer_default_directory)) dir = ibuffer_default_directory
                name = file_relative_name(name, buffers(cur)%directory)
                if (allocated(ibuffer_default_directory)) name = file_relative_name(name, ibuffer_default_directory)
            else
                name = file_name_nondirectory(name)
            end if

            if (len(string) > 0) string = string//" "
            string = string//name
        end do

        call copy_as_kill(string)
    end subroutine ibuffer_copy_filename_as_kill

    !> Copy buffer names of marked (or next ARG) buffers into the kill ring.
    !> The names are separated by a space.
    !> You can then feed the file name(s) to other commands with \[yank].
    !> Port of ibuf-ext.el:ibuffer-copy-buffername-as-kill.
    recursive subroutine ibuffer_copy_buffername_as_kill(status)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: bufs(:)
        character(len=:), allocatable :: string
        integer :: k

        call copy_buffers(bufs, status)
        if (.not. status%ok()) return
        string = ""

        do k = 1, size(bufs)
            if (k > 1) string = string//" "
            string = string//buffers(bufs(k)%index)%name
        end do

        call copy_as_kill(string)
    end subroutine ibuffer_copy_buffername_as_kill

    ! ------------------------------------------------------------------
    ! Marking
    ! ------------------------------------------------------------------

    ! Port of ibuf-ext.el:ibuffer-mark-on-buffer: mark the buffers for
    ! which FUNC is true with MARK (`ibuffer_marked_char' by default).
    recursive subroutine ibuffer_mark_on_buffer(func, status, mark, group)
        procedure(ibuffer_buffer_predicate_proc) :: func
        type(status_type), intent(out) :: status
        character, intent(in), optional :: mark
        character(len=*), intent(in), optional :: group
        procedure(ibuffer_buffer_predicate_proc), pointer :: saved_fn
        character :: m, saved_mark
        integer :: count
        character(len=24) :: c

        m = ibuffer_marked_char
        if (present(mark)) m = mark
        saved_fn => mark_on_fn
        saved_mark = map_mark
        mark_on_fn => func
        map_mark = m

        if (present(group)) then
            count = ibuffer_map_lines(mark_if, status, group=group)
        else
            count = ibuffer_map_lines(mark_if, status)
        end if

        mark_on_fn => saved_fn
        map_mark = saved_mark
        if (.not. status%ok()) return
        call ibuffer_redisplay(status, .true.)

        if (m /= " ") then
            write (c, "(i0)") count
            call message("Marked "//trim(c)//" buffers")
        end if
    end subroutine ibuffer_mark_on_buffer

    ! Port of part of ibuf-ext.el:ibuffer-mark-on-buffer.
    recursive integer function mark_if(buf, mark) result(r)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark

        r = IBUFFER_OP_SKIPPED
        if (mark == achar(0)) continue

        if (mark_on_fn(buf)) then
            call ibuffer_set_mark_1(map_mark)
            r = IBUFFER_OP_DONE
        end if
    end function mark_if

    ! (string-match MARK-ON-REGEXP TEXT)  Not a port.
    logical function regexp_matches(text)
        character(len=*), intent(in) :: text
        integer(int64) :: m
        type(status_type) :: st

        m = string_match(mark_on_regexp, text, st)
        regexp_matches = st%ok() .and. m > 0
    end function regexp_matches

    !> Mark all buffers whose name matches REGEXP.  Port of
    !> ibuf-ext.el:ibuffer-mark-by-name-regexp.
    recursive subroutine ibuffer_mark_by_name_regexp(regexp, status)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status

        mark_on_regexp = regexp
        call ibuffer_mark_on_buffer(name_matches, status)
    end subroutine ibuffer_mark_by_name_regexp

    ! Port of part of ibuf-ext.el:ibuffer-mark-by-name-regexp.
    recursive logical function name_matches(buf)
        type(handle_type), intent(in) :: buf

        name_matches = regexp_matches(buffers(buf%index)%name)
    end function name_matches

    recursive subroutine cmd_ibuffer_mark_by_name_regexp(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp

        call read_string_arg("Mark by name (regexp): ", regexp, status)
        if (status%ok()) call ibuffer_mark_by_name_regexp(regexp, status)
    end subroutine cmd_ibuffer_mark_by_name_regexp

    !> Mark all locked buffers.  (emacs-lock.el is not ported: there are
    !> none.)  Port of ibuf-ext.el:ibuffer-mark-by-locked.
    recursive subroutine ibuffer_mark_by_locked(status)
        type(status_type), intent(out) :: status

        status = status_type()
    end subroutine ibuffer_mark_by_locked

    !> Mark all buffers whose major mode matches REGEXP.  Port of
    !> ibuf-ext.el:ibuffer-mark-by-mode-regexp.
    recursive subroutine ibuffer_mark_by_mode_regexp(regexp, status)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status

        mark_on_regexp = regexp
        call ibuffer_mark_on_buffer(mode_matches, status)
    end subroutine ibuffer_mark_by_mode_regexp

    ! Port of part of ibuf-ext.el:ibuffer-mark-by-mode-regexp.
    recursive logical function mode_matches(buf)
        type(handle_type), intent(in) :: buf

        mode_matches = regexp_matches(mode_name_of(buf))
    end function mode_matches

    recursive subroutine cmd_ibuffer_mark_by_mode_regexp(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp

        call read_string_arg("Mark by major mode (regexp): ", regexp, status)
        if (status%ok()) call ibuffer_mark_by_mode_regexp(regexp, status)
    end subroutine cmd_ibuffer_mark_by_mode_regexp

    !> Mark all buffers whose file name matches REGEXP.  Port of
    !> ibuf-ext.el:ibuffer-mark-by-file-name-regexp.
    recursive subroutine ibuffer_mark_by_file_name_regexp(regexp, status)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status

        mark_on_regexp = regexp
        call ibuffer_mark_on_buffer(file_name_matches, status)
    end subroutine ibuffer_mark_by_file_name_regexp

    ! Port of part of ibuf-ext.el:ibuffer-mark-by-file-name-regexp.
    recursive logical function file_name_matches(buf)
        type(handle_type), intent(in) :: buf
        character(len=:), allocatable :: name
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call set_buffer(buf, st)
        name = ibuffer_buffer_file_name()
        call set_buffer(old, st)
        file_name_matches = .false.
        ! Match on the displayed file name (which is abbreviated).
        if (len(name) > 0) file_name_matches = regexp_matches(ibuffer_abbreviate_file_name(name))
    end function file_name_matches

    recursive subroutine cmd_ibuffer_mark_by_file_name_regexp(status)
        use replace_fm, only: read_regexp
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp

        call read_regexp("Mark by file name (regexp)", regexp, status)
        if (status%ok()) call ibuffer_mark_by_file_name_regexp(regexp, status)
    end subroutine cmd_ibuffer_mark_by_file_name_regexp

    !> Mark all buffers whose content matches REGEXP.
    !> Optional arg ALL_BUFFERS, if true, then search in all buffers.
    !> Otherwise buffers whose name matches an element of
    !> `ibuffer_never_search_content_name' or whose major mode is on
    !> `ibuffer_never_search_content_mode' are excluded.  Port of
    !> ibuf-ext.el:ibuffer-mark-by-content-regexp.
    recursive subroutine ibuffer_mark_by_content_regexp(regexp, status, all_buffers)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: all_buffers

        mark_on_regexp = regexp
        mark_on_all = .false.
        if (present(all_buffers)) mark_on_all = all_buffers
        call ibuffer_mark_on_buffer(content_matches, status)
    end subroutine ibuffer_mark_by_content_regexp

    ! Port of part of ibuf-ext.el:ibuffer-mark-by-content-regexp.
    recursive logical function content_matches(buf)
        type(handle_type), intent(in) :: buf
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: saved, found, m
        integer :: k

        content_matches = .false.

        if (.not. mark_on_all) then
            if (any(ibuffer_never_search_content_mode == major_mode_of(buf))) return

            do k = 1, size(ibuffer_never_search_content_name)
                m = string_match(trim(ibuffer_never_search_content_name(k)), buffers(buf%index)%name, st)
                if (st%ok() .and. m > 0) return
            end do
        end if

        old = current_buffer()
        call set_buffer(buf, st)
        saved = point()
        call goto_char(point_min())
        call re_search_forward(mark_on_regexp, found, st, noerror=1)
        content_matches = st%ok() .and. found /= 0
        call goto_char(saved)
        call set_buffer(old, st)
    end function content_matches

    recursive subroutine cmd_ibuffer_mark_by_content_regexp(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp

        call read_from_minibuffer("Mark by content (regexp): ", regexp, status)
        if (status%ok()) call ibuffer_mark_by_content_regexp(regexp, status, allocated(current_prefix_arg%a))
    end subroutine cmd_ibuffer_mark_by_content_regexp

    !> Mark all buffers whose major mode equals MODE.  Port of
    !> ibuf-ext.el:ibuffer-mark-by-mode.
    recursive subroutine ibuffer_mark_by_mode(mode, status)
        character(len=*), intent(in) :: mode
        type(status_type), intent(out) :: status

        mark_on_regexp = mode
        call ibuffer_mark_on_buffer(mode_is, status)
    end subroutine ibuffer_mark_by_mode

    ! Not a port.
    recursive logical function mode_is(buf)
        type(handle_type), intent(in) :: buf

        mode_is = major_mode_of(buf) == mark_on_regexp
    end function mode_is

    recursive subroutine cmd_ibuffer_mark_by_mode(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf
        type(completion_table_type) :: table
        type(value_type), allocatable :: names(:)
        character(len=IBUFFER_NAME_LEN), allocatable :: modes(:)
        character(len=:), allocatable :: default, prompt, mode
        type(handle_type), allocatable :: bufs(:)
        type(status_type) :: st
        integer :: k, n

        buf = ibuffer_current_buffer(st)
        default = ""
        if (buf%index /= 0) default = major_mode_of(buf)
        ! (ibuffer-list-buffer-modes: the major modes in use.)
        bufs = buffer_list()
        allocate (modes(size(bufs)))
        n = 0

        do k = 1, size(bufs)
            if (any(modes(1:n) == major_mode_of(bufs(k)))) cycle
            n = n + 1
            modes(n) = major_mode_of(bufs(k))
        end do

        allocate (names(n))

        do k = 1, n
            names(k) = value_type(text=trim(modes(k)))
        end do

        table%kind = TABLE_LIST
        table%list = make_list(names)

        if (len(default) > 0) then
            prompt = "Mark by major mode (default "//default//"): "
            call completing_read(prompt, table, mode, status, require_match=REQUIRE_MATCH_YES, default=default)
        else
            call completing_read("Mark by major mode: ", table, mode, status, require_match=REQUIRE_MATCH_YES)
        end if

        if (status%ok()) call ibuffer_mark_by_mode(mode, status)
    end subroutine cmd_ibuffer_mark_by_mode

    !> Mark all modified buffers.  Port of
    !> ibuf-ext.el:ibuffer-mark-modified-buffers.
    recursive subroutine ibuffer_mark_modified_buffers(status)
        type(status_type), intent(out) :: status

        call ibuffer_mark_on_buffer(is_modified, status)
    end subroutine ibuffer_mark_modified_buffers

    ! Port of part of ibuf-ext.el:ibuffer-mark-modified-buffers.
    recursive logical function is_modified(buf)
        type(handle_type), intent(in) :: buf
        type(status_type) :: st

        call is_buffer_modified(buf, is_modified, st)
    end function is_modified

    !> Mark all modified buffers that have an associated file.  Port of
    !> ibuf-ext.el:ibuffer-mark-unsaved-buffers.
    recursive subroutine ibuffer_mark_unsaved_buffers(status)
        type(status_type), intent(out) :: status

        call ibuffer_mark_on_buffer(is_unsaved, status)
    end subroutine ibuffer_mark_unsaved_buffers

    ! Port of part of ibuf-ext.el:ibuffer-mark-unsaved-buffers.
    recursive logical function is_unsaved(buf)
        type(handle_type), intent(in) :: buf

        is_unsaved = .false.

        if (allocated(buffers(buf%index)%filename)) then
            if (len(buffers(buf%index)%filename) > 0) is_unsaved = is_modified(buf)
        end if
    end function is_unsaved

    !> Mark all buffers whose associated file does not exist.  Port of
    !> ibuf-ext.el:ibuffer-mark-dissociated-buffers.
    recursive subroutine ibuffer_mark_dissociated_buffers(status)
        type(status_type), intent(out) :: status

        call ibuffer_mark_on_buffer(is_dissociated, status)
    end subroutine ibuffer_mark_dissociated_buffers

    ! Port of part of ibuf-ext.el:ibuffer-mark-dissociated-buffers.
    recursive logical function is_dissociated(buf)
        type(handle_type), intent(in) :: buf
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: dir

        is_dissociated = .false.

        if (allocated(buffers(buf%index)%filename)) then
            if (len(buffers(buf%index)%filename) > 0) then
                is_dissociated = .not. file_exists(buffers(buf%index)%filename)
                return
            end if
        end if

        if (major_mode_of(buf) /= "dired_mode") return
        old = current_buffer()
        call set_buffer(buf, st)
        dir = dired_directory_of()
        call set_buffer(old, st)
        if (len(dir) > 0) is_dissociated = .not. file_exists(file_name_directory(dir))
    end function is_dissociated

    !> Mark buffers whose major mode is in variable
    !> `ibuffer_help_buffer_modes'.  Port of
    !> ibuf-ext.el:ibuffer-mark-help-buffers.
    recursive subroutine ibuffer_mark_help_buffers(status)
        type(status_type), intent(out) :: status

        call ibuffer_mark_on_buffer(is_help, status)
    end subroutine ibuffer_mark_help_buffers

    ! Port of part of ibuf-ext.el:ibuffer-mark-help-buffers.
    recursive logical function is_help(buf)
        type(handle_type), intent(in) :: buf

        is_help = any(ibuffer_help_buffer_modes == major_mode_of(buf))
    end function is_help

    !> Mark buffers whose associated file is compressed.  Port of
    !> ibuf-ext.el:ibuffer-mark-compressed-file-buffers.
    recursive subroutine ibuffer_mark_compressed_file_buffers(status)
        type(status_type), intent(out) :: status

        call ibuffer_mark_on_buffer(is_compressed, status)
    end subroutine ibuffer_mark_compressed_file_buffers

    ! Port of part of ibuf-ext.el:ibuffer-mark-compressed-file-buffers.
    recursive logical function is_compressed(buf)
        type(handle_type), intent(in) :: buf
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call set_buffer(buf, st)
        is_compressed = form_compressed()
        call set_buffer(old, st)
    end function is_compressed

    !> Mark buffers which have not been viewed in `ibuffer_old_time' hours.
    !> Port of ibuf-ext.el:ibuffer-mark-old-buffers.
    recursive subroutine ibuffer_mark_old_buffers(status)
        type(status_type), intent(out) :: status

        call ibuffer_mark_on_buffer(is_old, status)
    end subroutine ibuffer_mark_old_buffers

    ! Port of part of ibuf-ext.el:ibuffer-mark-old-buffers.
    recursive logical function is_old(buf)
        type(handle_type), intent(in) :: buf
        integer(int64) :: sec
        integer(int32) :: nsec
        real(real64) :: since

        is_old = .false.
        if (.not. buffers(buf%index)%display_time_set) return
        call emacs_clock_realtime(sec, nsec)
        since = real(sec - buffers(buf%index)%display_time_sec, real64) &
                + real(nsec - buffers(buf%index)%display_time_nsec, real64)*1.0e-9_real64
        is_old = real(60*60*ibuffer_old_time, real64) < since
    end function is_old

    !> Mark all buffers whose name begins and ends with `*'.  Port of
    !> ibuf-ext.el:ibuffer-mark-special-buffers.
    recursive subroutine ibuffer_mark_special_buffers(status)
        type(status_type), intent(out) :: status

        mark_on_regexp = "^\*.+\*$"
        call ibuffer_mark_on_buffer(name_matches, status)
    end subroutine ibuffer_mark_special_buffers

    !> Mark all read-only buffers.  Port of
    !> ibuf-ext.el:ibuffer-mark-read-only-buffers.
    recursive subroutine ibuffer_mark_read_only_buffers(status)
        type(status_type), intent(out) :: status

        call ibuffer_mark_on_buffer(is_read_only, status)
    end subroutine ibuffer_mark_read_only_buffers

    ! Port of part of ibuf-ext.el:ibuffer-mark-read-only-buffers.
    recursive logical function is_read_only(buf)
        type(handle_type), intent(in) :: buf

        is_read_only = buffers(buf%index)%read_only
    end function is_read_only

    !> Mark all `dired' buffers.  Port of
    !> ibuf-ext.el:ibuffer-mark-dired-buffers.
    recursive subroutine ibuffer_mark_dired_buffers(status)
        type(status_type), intent(out) :: status

        mark_on_regexp = "dired_mode"
        call ibuffer_mark_on_buffer(mode_is, status)
    end subroutine ibuffer_mark_dired_buffers

    !> View lines which match REGEXP in all marked buffers.
    !> Optional argument NLINES says how many lines of context to display:
    !> it defaults to one.  Port of ibuf-ext.el:ibuffer-do-occur.
    recursive subroutine ibuffer_do_occur(regexp, status, nlines)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: nlines
        type(occur_source_type), allocatable :: sources(:)
        integer(int64) :: n
        integer :: count, k

        n = 0
        if (present(nlines)) n = nlines
        if (n < 0) n = 0
        if (ibuffer_count_marked_lines(status) == 0) call ibuffer_set_mark(ibuffer_marked_char, status)
        if (.not. status%ok()) return
        ! Accumulate a list of marked buffers
        map_nstates = 0
        if (allocated(map_states)) deallocate (map_states)
        allocate (map_states(16))
        count = map_marked_lines(add_buffer, status)
        if (.not. status%ok()) return
        ! (Pushed: the last marked buffer first.)
        allocate (sources(map_nstates))

        do k = 1, map_nstates
            sources(k)%buffer = map_states(map_nstates - k + 1)%buffer
        end do

        call occur_1(regexp, .true., n, sources, status)
    end subroutine ibuffer_do_occur

    ! Port of part of ibuf-ext.el:ibuffer-do-occur.
    recursive integer function add_buffer(buf, mark) result(r)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        type(ibuffer_state_type), allocatable :: grown(:)

        if (mark == achar(0)) continue

        if (map_nstates == size(map_states)) then
            allocate (grown(2*size(map_states)))
            grown(1:map_nstates) = map_states(1:map_nstates)
            call move_alloc(grown, map_states)
        end if

        map_nstates = map_nstates + 1
        map_states(map_nstates)%buffer = buf
        r = IBUFFER_OP_DONE
    end function add_buffer

    recursive subroutine cmd_ibuffer_do_occur(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp, collect
        logical :: has_nlines, has_collect
        integer(int64) :: nlines

        call occur_read_primary_args(regexp, has_nlines, nlines, has_collect, collect, status)
        if (.not. status%ok()) return

        if (has_nlines) then
            call ibuffer_do_occur(regexp, status, nlines)
        else
            call ibuffer_do_occur(regexp, status)
        end if
    end subroutine cmd_ibuffer_do_occur

    ! Rename the current buffer to a similar name not already taken.
    ! Port of files.el:rename-uniquely (not interactive).
    subroutine rename_uniquely()
        use buffer_fm, only: rename_buffer, generate_new_buffer_name
        character(len=:), allocatable :: base_name, actual
        integer(int64) :: idx
        type(status_type) :: st

        base_name = buffers(cur)%name
        idx = string_match("<[0-9]+>\'", base_name, st)

        if (idx /= 0) then
            ! If the existing buffer name has a <NNN>, which isn't part of
            ! the file name (if any), then get rid of that.
            if (.not. allocated(buffers(cur)%filename)) then
                base_name = base_name(1:idx - 1)
            else if (base_name /= file_name_nondirectory(buffers(cur)%filename)) then
                base_name = base_name(1:idx - 1)
            end if
        end if

        call rename_buffer(generate_new_buffer_name(base_name), .false., actual, st)
    end subroutine rename_uniquely

    ! ------------------------------------------------------------------
    ! Filter definitions (define-ibuffer-filter)
    ! ------------------------------------------------------------------

    ! The string qualifier of NODE, an error if it has none.  Not a port.
    subroutine string_qualifier(node, s, status)
        type(ibuffer_filter_node_type), intent(in) :: node
        character(len=:), allocatable, intent(out) :: s
        type(status_type), intent(out) :: status

        status = status_type()
        s = qualifier_text(node)

        if (.not. node%has_qualifier .or. node%is_number .or. associated(node%predicate)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected character(len=*)")
        end if
    end subroutine string_qualifier

    ! (string-match QUALIFIER STRING).  Not a port.
    recursive logical function qualifier_matches(node, string, status) result(hit)
        type(ibuffer_filter_node_type), intent(in) :: node
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp

        hit = .false.
        call string_qualifier(node, regexp, status)
        if (.not. status%ok()) return
        hit = string_match(regexp, string, status) > 0
    end function qualifier_matches

    ! The `ibuffer_buffer_file_name' of BUF.  Not a port.
    function file_name_of(buf) result(name)
        type(handle_type), intent(in) :: buf
        character(len=:), allocatable :: name
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call set_buffer(buf, st)
        name = ibuffer_buffer_file_name()
        call set_buffer(old, st)
    end function file_name_of

    ! Port of ibuf-ext.el define-ibuffer-filter mode and used-mode.
    recursive logical function filter_mode(buf, qualifier, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: qualifier
        type(status_type), intent(out) :: status

        status = status_type()
        included = qualifier%has_qualifier .and. .not. qualifier%is_number
        if (included) included = major_mode_of(buf) == qualifier_text(qualifier)
    end function filter_mode

    ! Port of ibuf-ext.el define-ibuffer-filter derived-mode.
    recursive logical function filter_derived_mode(buf, qualifier, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: qualifier
        type(status_type), intent(out) :: status

        status = status_type()
        included = qualifier%has_qualifier .and. .not. qualifier%is_number
        if (included) included = provided_mode_derived_p(major_mode_of(buf), qualifier_text(qualifier))
    end function filter_derived_mode

    ! Port of ibuf-ext.el define-ibuffer-filter name.
    recursive logical function filter_name(buf, qualifier, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: qualifier
        type(status_type), intent(out) :: status

        included = qualifier_matches(qualifier, buffers(buf%index)%name, status)
    end function filter_name

    ! Port of ibuf-ext.el define-ibuffer-filter process.
    recursive logical function filter_process(buf, qualifier, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: qualifier
        type(status_type), intent(out) :: status
        type(handle_type) :: proc

        status = status_type()
        if (qualifier%has_qualifier) continue
        proc = get_buffer_process(buf)
        included = proc%index /= 0
    end function filter_process

    ! Port of ibuf-ext.el define-ibuffer-filter starred-name.
    recursive logical function filter_starred_name(buf, qualifier, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: qualifier
        type(status_type), intent(out) :: status

        if (qualifier%has_qualifier) continue
        included = string_match("\`\*[^*]+\*\(?:<[[:digit:]]+>\)?\'", buffers(buf%index)%name, status) > 0
    end function filter_starred_name

    ! Port of ibuf-ext.el define-ibuffer-filter filename.
    recursive logical function filter_filename(buf, qualifier, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: qualifier
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: it

        status = status_type()
        included = .false.
        it = file_name_of(buf)
        if (len(it) > 0) included = qualifier_matches(qualifier, it, status)
    end function filter_filename

    ! Port of ibuf-ext.el define-ibuffer-filter basename.
    recursive logical function filter_basename(buf, qualifier, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: qualifier
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: it

        status = status_type()
        included = .false.
        it = file_name_of(buf)
        if (len(it) > 0) included = qualifier_matches(qualifier, file_name_nondirectory(it), status)
    end function filter_basename

    ! Port of ibuf-ext.el define-ibuffer-filter file-extension.
    recursive logical function filter_file_extension(buf, qualifier, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: qualifier
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: it, ext
        logical :: found

        status = status_type()
        included = .false.
        it = file_name_of(buf)
        if (len(it) == 0) return
        call file_name_extension(it, ext, found)
        if (.not. found) ext = ""
        included = qualifier_matches(qualifier, ext, status)
    end function filter_file_extension

    ! Port of ibuf-ext.el define-ibuffer-filter directory.
    recursive logical function filter_directory(buf, qualifier, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: qualifier
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, dirname

        status = status_type()
        included = .false.
        filename = file_name_of(buf)
        dirname = ""
        if (len(filename) > 0) dirname = file_name_directory(filename)

        if (len(dirname) > 0) then
            included = qualifier_matches(qualifier, dirname, status)
        else if (allocated(buffers(buf%index)%directory)) then
            included = qualifier_matches(qualifier, buffers(buf%index)%directory, status)
        end if
    end function filter_directory

    ! The size of BUF and the number qualifier of NODE, an error if it has
    ! none.  Not a port.
    subroutine size_and_number(buf, node, size, number, status)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: node
        integer(int64), intent(out) :: size, number
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st

        status = status_type()
        old = current_buffer()
        call set_buffer(buf, st)
        size = buffer_size()
        call set_buffer(old, st)
        number = node%number
        if (.not. node%is_number) call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer, real or type(marker_type)")
    end subroutine size_and_number

    ! Port of ibuf-ext.el define-ibuffer-filter size-gt.
    recursive logical function filter_size_gt(buf, qualifier, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: qualifier
        type(status_type), intent(out) :: status
        integer(int64) :: size, number

        call size_and_number(buf, qualifier, size, number, status)
        included = status%ok() .and. size > number
    end function filter_size_gt

    ! Port of ibuf-ext.el define-ibuffer-filter size-lt.
    recursive logical function filter_size_lt(buf, qualifier, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: qualifier
        type(status_type), intent(out) :: status
        integer(int64) :: size, number

        call size_and_number(buf, qualifier, size, number, status)
        included = status%ok() .and. size < number
    end function filter_size_lt

    ! Port of ibuf-ext.el define-ibuffer-filter modified.
    recursive logical function filter_modified(buf, qualifier, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: qualifier
        type(status_type), intent(out) :: status

        if (qualifier%has_qualifier) continue
        call is_buffer_modified(buf, included, status)
    end function filter_modified

    ! Port of ibuf-ext.el define-ibuffer-filter visiting-file.
    recursive logical function filter_visiting_file(buf, qualifier, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: qualifier
        type(status_type), intent(out) :: status

        status = status_type()
        if (qualifier%has_qualifier) continue
        included = .false.
        if (allocated(buffers(buf%index)%filename)) included = len(buffers(buf%index)%filename) > 0
    end function filter_visiting_file

    ! Port of ibuf-ext.el define-ibuffer-filter content.
    recursive logical function filter_content(buf, qualifier, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: qualifier
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: saved, found

        included = .false.
        call string_qualifier(qualifier, regexp, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buf, st)
        saved = point()
        call goto_char(point_min())
        call re_search_forward(regexp, found, status, noerror=SEARCH_NOERROR)
        included = status%ok() .and. found > 0
        call goto_char(saved)
        call set_buffer(old, st)
    end function filter_content

    ! Port of ibuf-ext.el define-ibuffer-filter predicate.
    recursive logical function filter_predicate(buf, qualifier, status) result(included)
        type(handle_type), intent(in) :: buf
        type(ibuffer_filter_node_type), intent(in) :: qualifier
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st

        status = status_type()
        included = .false.

        if (.not. associated(qualifier%predicate)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected procedure")
            return
        end if

        old = current_buffer()
        call set_buffer(buf, st)
        included = qualifier%predicate(buf)
        if (is_buffer_live(old)) call set_buffer(old, st)
    end function filter_predicate

    ! ------------------------------------------------------------------
    ! The filter stack
    ! ------------------------------------------------------------------

    ! Jump back to BUF after an update (it may have been filtered out).
    ! Not a port.
    recursive subroutine jump_back(buf, status)
        type(handle_type), intent(in) :: buf
        type(status_type), intent(out) :: status

        status = status_type()
        if (buf%index == 0) return
        if (.not. is_buffer_live(buf)) return
        call ibuffer_jump_to_buffer(buffers(buf%index)%name, status)
    end subroutine jump_back

    module procedure ibuffer_filter_disable
    type(handle_type) :: buf
    type(status_type) :: st
    integer :: k
    logical :: delete

    delete = .false.
    if (present(delete_filter_groups)) delete = delete_filter_groups
    k = ibuffer_local_index(.true.)
    deallocate (locals(k)%qualifiers)
    allocate (locals(k)%qualifiers(0))
    if (delete) call locals(k)%filter_groups%clear()
    buf = ibuffer_current_buffer(st)
    call ibuffer_update(status, .false., .true.)
    if (.not. status%ok()) return
    call jump_back(buf, status)
    end procedure ibuffer_filter_disable

    recursive subroutine cmd_ibuffer_filter_disable(status)
        type(status_type), intent(out) :: status

        call ibuffer_filter_disable(status)
    end subroutine cmd_ibuffer_filter_disable

    module procedure ibuffer_pop_filter
    type(ibuffer_filter_type), allocatable :: filters(:)
    type(handle_type) :: buf
    type(status_type) :: st

    filters = ibuffer_filtering_qualifiers()

    if (size(filters) == 0) then
        call set_error(status, FM_GENERIC_ERROR, "No filters in effect")
        return
    end if

    call ibuffer_set_filtering_qualifiers(filters(2:))
    buf = ibuffer_current_buffer(st)
    call ibuffer_update(status, .false., .true.)
    if (.not. status%ok()) return
    call jump_back(buf, status)
    end procedure ibuffer_pop_filter

    module procedure ibuffer_decompose_filter
    type(ibuffer_filter_type), allocatable :: filters(:), parts(:), saved(:), tail(:)
    type(status_type) :: st
    integer :: i

    filters = ibuffer_filtering_qualifiers()

    if (size(filters) == 0) then
        call set_error(status, FM_GENERIC_ERROR, "No filters in effect")
        return
    end if

    tail = filters(2:)

    select case (filters(1)%nodes(1)%kind)
    case (IBUFFER_FILTER_OR, IBUFFER_FILTER_AND)
        call operands(filters(1), parts)
        call ibuffer_set_filtering_qualifiers(joined(parts, tail))
    case (IBUFFER_FILTER_SAVED)
        i = ibuffer_saved_filters%find(qualifier_text(filters(1)%nodes(1)))

        if (i == 0) then
            call ibuffer_filter_disable(st)
            call set_error(status, FM_GENERIC_ERROR, "Unknown saved filter "//qualifier_text(filters(1)%nodes(1)))
            return
        end if

        saved = ibuffer_saved_filters%entries(i)%filters
        call ibuffer_set_filtering_qualifiers(joined(saved, tail))
    case (IBUFFER_FILTER_NOT)
        call add_front(tail, ibuffer_unary_operand(filters(1)))
        call ibuffer_set_filtering_qualifiers(tail)
    case default
        call set_error(status, FM_GENERIC_ERROR, "Filter type "//trim(filters(1)%nodes(1)%name)//" is not compound")
        return
    end select

    call ibuffer_update(status, .false., .true.)
    end procedure ibuffer_decompose_filter

    recursive subroutine cmd_ibuffer_decompose_filter(status)
        type(status_type), intent(out) :: status

        call ibuffer_decompose_filter(status)
    end subroutine cmd_ibuffer_decompose_filter

    module procedure ibuffer_exchange_filters
    type(ibuffer_filter_type), allocatable :: filters(:)
    type(ibuffer_filter_type) :: first

    filters = ibuffer_filtering_qualifiers()

    if (size(filters) < 2) then
        call set_error(status, FM_GENERIC_ERROR, "Need two filters to exchange")
        return
    end if

    first = filters(1)
    filters(1) = filters(2)
    filters(2) = first
    call ibuffer_set_filtering_qualifiers(filters)
    call ibuffer_update(status, .false., .true.)
    end procedure ibuffer_exchange_filters

    recursive subroutine cmd_ibuffer_exchange_filters(status)
        type(status_type), intent(out) :: status

        call ibuffer_exchange_filters(status)
    end subroutine cmd_ibuffer_exchange_filters

    module procedure ibuffer_negate_filter
    type(ibuffer_filter_type), allocatable :: filters(:)
    type(ibuffer_filter_type) :: lim(1)

    filters = ibuffer_filtering_qualifiers()

    if (size(filters) == 0) then
        call set_error(status, FM_GENERIC_ERROR, "No filters in effect")
        return
    end if

    if (filters(1)%nodes(1)%kind == IBUFFER_FILTER_NOT) then
        lim(1) = filters(1)
        filters(1) = ibuffer_unary_operand(lim(1))
    else
        lim(1) = filters(1)
        filters(1) = compound(IBUFFER_FILTER_NOT, lim)
    end if

    call ibuffer_set_filtering_qualifiers(filters)
    call ibuffer_update(status, .false., .true.)
    end procedure ibuffer_negate_filter

    recursive subroutine cmd_ibuffer_negate_filter(status)
        type(status_type), intent(out) :: status

        call ibuffer_negate_filter(status)
    end subroutine cmd_ibuffer_negate_filter

    ! Port of ibuf-ext.el:ibuffer--or-and-filter.
    recursive subroutine or_and_filter(op, decompose, status)
        integer, intent(in) :: op
        logical, intent(in) :: decompose
        type(status_type), intent(out) :: status
        type(ibuffer_filter_type), allocatable :: filters(:), a(:), b(:)
        character(len=3) :: opname
        integer :: top

        opname = "AND"
        if (op == IBUFFER_FILTER_OR) opname = "OR"
        filters = ibuffer_filtering_qualifiers()

        if (decompose) then
            top = 0
            if (size(filters) > 0) top = filters(1)%nodes(1)%kind

            if (top == op) then
                call ibuffer_decompose_filter(status)
            else
                call set_error(status, FM_GENERIC_ERROR, "Top filter is not an "//trim(opname))
            end if

            return
        end if

        if (size(filters) < 2) then
            call set_error(status, FM_GENERIC_ERROR, "Need two filters to "//trim(opname))
            return
        end if

        ! If either filter is an op, eliminate unnecessary nesting.
        if (filters(1)%nodes(1)%kind == op) then
            call operands(filters(1), a)
        else
            allocate (a(1))
            a(1) = filters(1)
        end if

        if (filters(2)%nodes(1)%kind == op) then
            call operands(filters(2), b)
        else
            allocate (b(1))
            b(1) = filters(2)
        end if

        filters = filters(2:)
        filters(1) = compound(op, joined(a, b))
        call ibuffer_set_filtering_qualifiers(filters)
        call ibuffer_update(status, .false., .true.)
    end subroutine or_and_filter

    module procedure ibuffer_or_filter
    logical :: d

    d = .false.
    if (present(decompose)) d = decompose
    call or_and_filter(IBUFFER_FILTER_OR, d, status)
    end procedure ibuffer_or_filter

    recursive subroutine cmd_ibuffer_or_filter(status)
        type(status_type), intent(out) :: status

        call ibuffer_or_filter(status, allocated(current_prefix_arg%a))
    end subroutine cmd_ibuffer_or_filter

    module procedure ibuffer_and_filter
    logical :: d

    d = .false.
    if (present(decompose)) d = decompose
    call or_and_filter(IBUFFER_FILTER_AND, d, status)
    end procedure ibuffer_and_filter

    recursive subroutine cmd_ibuffer_and_filter(status)
        type(status_type), intent(out) :: status

        call ibuffer_and_filter(status, allocated(current_prefix_arg%a))
    end subroutine cmd_ibuffer_and_filter

    ! ------------------------------------------------------------------
    ! Saved filters
    ! ------------------------------------------------------------------

    ! ibuffer-maybe-save-stuff: with `ibuffer_save_with_custom', Customize
    ! saves `ibuffer_saved_filters' and `ibuffer_saved_filter_groups'
    ! (Deferred with Customize, step 26).  Not a port.
    subroutine maybe_save_stuff()
    end subroutine maybe_save_stuff

    ! A completion table of the names of NAMES.  Not a port.
    function names_table(names) result(table)
        type(minibuffer_string_type), intent(in) :: names(:)
        type(completion_table_type) :: table
        type(value_type), allocatable :: values(:)
        integer :: j

        allocate (values(size(names)))

        do j = 1, size(names)
            values(j) = value_type(text=names(j)%s)
        end do

        table%kind = TABLE_LIST
        table%list = make_list(values)
    end function names_table

    ! The names of the saved filters.  Not a port.
    function saved_filter_names() result(names)
        type(minibuffer_string_type), allocatable :: names(:)
        integer :: j

        allocate (names(ibuffer_saved_filters%count()))

        do j = 1, size(names)
            names(j)%s = ibuffer_saved_filters%entries(j)%name
        end do
    end function saved_filter_names

    ! Read the name of a saved filter with PROMPT ("No saved filters" if
    ! there are none).  Not a port.
    recursive subroutine read_saved_filter_name(prompt, name, status)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status

        if (ibuffer_saved_filters%count() == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No saved filters")
            name = ""
            return
        end if

        call completing_read(prompt, names_table(saved_filter_names()), name, status, require_match=REQUIRE_MATCH_YES)
    end subroutine read_saved_filter_name

    module procedure ibuffer_save_filters
    status = status_type()
    call ibuffer_saved_filters%set(name, filters, front=.true.)
    call maybe_save_stuff()
    end procedure ibuffer_save_filters

    recursive subroutine cmd_ibuffer_save_filters(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        type(ibuffer_filter_type), allocatable :: filters(:)

        filters = ibuffer_filtering_qualifiers()

        if (size(filters) == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No filters currently in effect")
            return
        end if

        call read_from_minibuffer("Save current filters as: ", name, status)
        if (status%ok()) call ibuffer_save_filters(name, filters, status)
    end subroutine cmd_ibuffer_save_filters

    module procedure ibuffer_delete_saved_filters
    call ibuffer_saved_filters%remove(name)
    call maybe_save_stuff()
    call ibuffer_update(status, .false., .true.)
    end procedure ibuffer_delete_saved_filters

    recursive subroutine cmd_ibuffer_delete_saved_filters(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_saved_filter_name("Delete saved filters: ", name, status)
        if (status%ok()) call ibuffer_delete_saved_filters(name, status)
    end subroutine cmd_ibuffer_delete_saved_filters

    module procedure ibuffer_add_saved_filters
    type(ibuffer_filter_type), allocatable :: filters(:)

    filters = ibuffer_filtering_qualifiers()
    call add_front(filters, ibuffer_saved(name))
    call ibuffer_set_filtering_qualifiers(filters)
    call ibuffer_update(status, .false., .true.)
    end procedure ibuffer_add_saved_filters

    recursive subroutine cmd_ibuffer_add_saved_filters(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_saved_filter_name("Add saved filters: ", name, status)
        if (status%ok()) call ibuffer_add_saved_filters(name, status)
    end subroutine cmd_ibuffer_add_saved_filters

    module procedure ibuffer_switch_to_saved_filters
    type(ibuffer_filter_type) :: filters(1)

    filters(1) = ibuffer_saved(name)
    call ibuffer_set_filtering_qualifiers(filters)
    call ibuffer_update(status, .false., .true.)
    end procedure ibuffer_switch_to_saved_filters

    recursive subroutine cmd_ibuffer_switch_to_saved_filters(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_saved_filter_name("Switch to saved filters: ", name, status)
        if (status%ok()) call ibuffer_switch_to_saved_filters(name, status)
    end subroutine cmd_ibuffer_switch_to_saved_filters

    ! ------------------------------------------------------------------
    ! Filter groups
    ! ------------------------------------------------------------------

    module procedure ibuffer_filters_to_filter_group
    type(ibuffer_filter_type), allocatable :: filters(:)
    integer :: k

    filters = ibuffer_filtering_qualifiers()

    if (size(filters) == 0) then
        call set_error(status, FM_GENERIC_ERROR, "No filters in effect")
        return
    end if

    k = ibuffer_local_index(.true.)
    call locals(k)%filter_groups%insert(name, filters, 1)
    call ibuffer_filter_disable(status)
    end procedure ibuffer_filters_to_filter_group

    recursive subroutine cmd_ibuffer_filters_to_filter_group(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_from_minibuffer("Name for filtering group: ", name, status)
        if (status%ok()) call ibuffer_filters_to_filter_group(name, status)
    end subroutine cmd_ibuffer_filters_to_filter_group

    module procedure ibuffer_set_filter_groups_by_mode
    type(handle_type), allocatable :: bufs(:)
    character(len=IBUFFER_NAME_LEN), allocatable :: modes(:)
    character(len=:), allocatable :: mode
    type(ibuffer_filter_groups_type) :: groups
    integer :: j, n

    bufs = buffer_list()
    allocate (modes(size(bufs)))
    n = 0

    do j = 1, size(bufs)
        mode = major_mode_of(bufs(j))
        if (any(modes(1:n) == mode)) cycle
        if (.not. ibuffer_view_ibuffer .and. mode == "ibuffer_mode") cycle
        n = n + 1
        modes(n) = mode
    end do

    allocate (groups%groups(0))

    do j = 1, n
        call groups%add(trim(modes(j)), ibuffer_major_mode(trim(modes(j))))
    end do

    call ibuffer_set_filter_groups(groups)
    call ibuffer_update(status, .false., .true.)
    end procedure ibuffer_set_filter_groups_by_mode

    recursive subroutine cmd_ibuffer_set_filter_groups_by_mode(status)
        type(status_type), intent(out) :: status

        call ibuffer_set_filter_groups_by_mode(status)
    end subroutine cmd_ibuffer_set_filter_groups_by_mode

    ! Error "No filter groups active" if there are none.  Not a port.
    logical function no_filter_groups(status) result(none)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        k = ibuffer_local_index(.true.)
        none = locals(k)%filter_groups%count() == 0
        if (none) call set_error(status, FM_GENERIC_ERROR, "No filter groups active")
    end function no_filter_groups

    module procedure ibuffer_pop_filter_group
    integer :: k

    if (no_filter_groups(status)) return
    k = ibuffer_local_index(.true.)
    ! (Upstream deletes the popped group, not its name, from
    ! `ibuffer_hidden_filter_groups', which leaves them as they are.)
    call locals(k)%filter_groups%remove(locals(k)%filter_groups%groups(1)%name)
    call ibuffer_update(status, .false., .true.)
    end procedure ibuffer_pop_filter_group

    recursive subroutine cmd_ibuffer_pop_filter_group(status)
        type(status_type), intent(out) :: status

        call ibuffer_pop_filter_group(status)
    end subroutine cmd_ibuffer_pop_filter_group

    ! Port of ibuf-ext.el:ibuffer-read-filter-group-name.
    recursive subroutine read_filter_group_name(msg, name, status, nodefault, noerror)
        character(len=*), intent(in) :: msg
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: nodefault, noerror
        type(ibuffer_state_type), allocatable :: states(:)
        character(len=256), allocatable :: groups(:)
        integer, allocatable :: group_of(:)
        type(minibuffer_string_type), allocatable :: names(:)
        logical :: no_default, no_error
        integer :: k, j, n

        no_default = .false.
        if (present(nodefault)) no_default = nodefault
        no_error = .false.
        if (present(noerror)) no_error = noerror
        name = ""

        if (.not. no_error) then
            if (no_filter_groups(status)) return
        end if

        ! `ibuffer_generate_filter_groups' returns all non-hidden filter
        ! groups, possibly excluding empty groups or Default.
        ! We add `ibuffer_hidden_filter_groups' to the list, excluding
        ! Default if necessary.
        call ibuffer_current_state_list(states, status)
        if (.not. status%ok()) return
        call ibuffer_generate_filter_groups(states, groups, group_of, status,.not. ibuffer_show_empty_filter_groups, &
                                            no_default)
        if (.not. status%ok()) return
        k = ibuffer_local_index(.true.)
        allocate (names(size(groups) + size(locals(k)%hidden_groups)))
        n = 0

        do j = 1, size(groups)
            n = n + 1
            names(n)%s = trim(groups(j))
        end do

        do j = 1, size(locals(k)%hidden_groups)
            if (no_default .and. locals(k)%hidden_groups(j) == "Default") cycle
            n = n + 1
            names(n)%s = trim(locals(k)%hidden_groups(j))
        end do

        call completing_read(msg, names_table(names(1:n)), name, status, require_match=REQUIRE_MATCH_YES)
    end subroutine read_filter_group_name

    module procedure ibuffer_decompose_filter_group
    type(ibuffer_filter_type), allocatable :: data(:)
    integer :: k, i

    k = ibuffer_local_index(.true.)
    i = locals(k)%filter_groups%find(group)

    if (i > 0) then
        data = locals(k)%filter_groups%groups(i)%filters
    else
        allocate (data(0))
    end if

    call locals(k)%filter_groups%remove(group)
    call ibuffer_set_filtering_qualifiers(data)
    call ibuffer_update(status, .false., .true.)
    end procedure ibuffer_decompose_filter_group

    recursive subroutine cmd_ibuffer_decompose_filter_group(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: group

        call read_filter_group_name("Decompose filter group: ", group, status, nodefault=.true.)
        if (status%ok()) call ibuffer_decompose_filter_group(group, status)
    end subroutine cmd_ibuffer_decompose_filter_group

    module procedure ibuffer_clear_filter_groups
    integer :: k

    k = ibuffer_local_index(.true.)
    call locals(k)%filter_groups%clear()
    deallocate (locals(k)%hidden_groups)
    allocate (locals(k)%hidden_groups(0))
    call ibuffer_update(status, .false., .true.)
    end procedure ibuffer_clear_filter_groups

    recursive subroutine cmd_ibuffer_clear_filter_groups(status)
        type(status_type), intent(out) :: status

        call ibuffer_clear_filter_groups(status)
    end subroutine cmd_ibuffer_clear_filter_groups

    module procedure ibuffer_kill_filter_group
    type(ibuffer_saved_filter_type), allocatable :: ring(:)
    integer :: k, i, j, n

    if (name == "Default") then
        call set_error(status, FM_GENERIC_ERROR, "Can't kill default filter group")
        return
    end if

    k = ibuffer_local_index(.true.)
    i = locals(k)%filter_groups%find(name)

    if (i == 0) then
        call set_error(status, FM_GENERIC_ERROR, 'No filter group with name "'//name//'"')
        return
    end if

    n = size(locals(k)%group_kill_ring)
    allocate (ring(n + 1))
    ring(1) = locals(k)%filter_groups%groups(i)

    do j = 1, n
        ring(j + 1) = locals(k)%group_kill_ring(j)
    end do

    call move_alloc(ring, locals(k)%group_kill_ring)
    call locals(k)%filter_groups%remove(name)
    locals(k)%hidden_groups = pack(locals(k)%hidden_groups, locals(k)%hidden_groups /= name)
    call ibuffer_update(status, .false., .true.)
    end procedure ibuffer_kill_filter_group

    recursive subroutine cmd_ibuffer_kill_filter_group(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_filter_group_name("Kill filter group: ", name, status, nodefault=.true.)
        if (status%ok()) call ibuffer_kill_filter_group(name, status)
    end subroutine cmd_ibuffer_kill_filter_group

    !> Kill the filter group at point.
    !> See also `ibuffer_kill_filter_group'.  Port of
    !> ibuf-ext.el:ibuffer-kill-line.
    recursive subroutine ibuffer_kill_line(status)
        type(status_type), intent(out) :: status
        integer(int64) :: saved
        logical :: found
        character(len=:), allocatable :: it

        saved = point()
        call ibuffer_forward_line(0_int64)
        found = has_string_prop(point(), "ibuffer_filter_group_name")
        if (found) it = string_prop(point(), "ibuffer_filter_group_name")
        call goto_char(saved)

        if (found) then
            call ibuffer_kill_filter_group(it, status)
        else if (.not. allocated(current_prefix_arg%a)) then
            call simple_kill_line(1_int64, .false., status)
        else
            call simple_kill_line(prefix_numeric_value(current_prefix_arg), .true., status)
        end if
    end subroutine ibuffer_kill_line

    ! Port of ibuf-ext.el:ibuffer-insert-filter-group-before.
    subroutine insert_filter_group_before(newgroup, group)
        type(ibuffer_saved_filter_type), intent(in) :: newgroup
        character(len=*), intent(in) :: group
        integer :: k, pos

        k = ibuffer_local_index(.true.)
        pos = locals(k)%filter_groups%find(group)
        if (pos == 0) pos = locals(k)%filter_groups%count() + 1
        call locals(k)%filter_groups%insert(newgroup%name, newgroup%filters, pos)
    end subroutine insert_filter_group_before

    !> Yank the last killed filter group before group at point.  Port of
    !> ibuf-ext.el:ibuffer-yank.
    recursive subroutine ibuffer_yank(status)
        type(status_type), intent(out) :: status

        if (has_string_prop(point(), "ibuffer_filter_group_name")) then
            call ibuffer_yank_filter_group(string_prop(point(), "ibuffer_filter_group_name"), status)
        else if (has_string_prop(point(), "ibuffer_filter_group")) then
            call ibuffer_yank_filter_group(string_prop(point(), "ibuffer_filter_group"), status)
        else
            call set_error(status, FM_GENERIC_ERROR, "No filter group at point")
        end if
    end subroutine ibuffer_yank

    module procedure ibuffer_yank_filter_group
    type(ibuffer_saved_filter_type) :: top
    integer :: k

    k = ibuffer_local_index(.true.)

    if (size(locals(k)%group_kill_ring) == 0) then
        call set_error(status, FM_GENERIC_ERROR, "The Ibuffer filter group kill-ring is empty")
        return
    end if

    top = locals(k)%group_kill_ring(1)
    locals(k)%group_kill_ring = locals(k)%group_kill_ring(2:)
    call insert_filter_group_before(top, name)
    call ibuffer_update(status, .false., .true.)
    end procedure ibuffer_yank_filter_group

    recursive subroutine cmd_ibuffer_yank_filter_group(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_filter_group_name("Yank filter group before group: ", name, status)
        if (status%ok()) call ibuffer_yank_filter_group(name, status)
    end subroutine cmd_ibuffer_yank_filter_group

    module procedure ibuffer_save_filter_groups
    status = status_type()
    call ibuffer_saved_filter_groups%add(name, groups, front=.true.)
    call maybe_save_stuff()
    end procedure ibuffer_save_filter_groups

    recursive subroutine cmd_ibuffer_save_filter_groups(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        if (no_filter_groups(status)) return
        call read_from_minibuffer("Save current filter groups as: ", name, status)
        if (status%ok()) call ibuffer_save_filter_groups(name, ibuffer_filter_groups(), status)
    end subroutine cmd_ibuffer_save_filter_groups

    ! The names of the saved sets of filter groups.  Not a port.
    function saved_group_names() result(names)
        type(minibuffer_string_type), allocatable :: names(:)
        integer :: j

        allocate (names(ibuffer_saved_filter_groups%count()))

        do j = 1, size(names)
            names(j)%s = ibuffer_saved_filter_groups%entries(j)%name
        end do
    end function saved_group_names

    module procedure ibuffer_delete_saved_filter_groups
    call ibuffer_saved_filter_groups%remove(name)
    call maybe_save_stuff()
    call ibuffer_update(status, .false., .true.)
    end procedure ibuffer_delete_saved_filter_groups

    recursive subroutine cmd_ibuffer_delete_saved_filter_groups(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        if (ibuffer_saved_filter_groups%count() == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No saved filter groups")
            return
        end if

        call completing_read("Delete saved filter group: ", names_table(saved_group_names()), name, status, &
                             require_match=REQUIRE_MATCH_YES)
        if (status%ok()) call ibuffer_delete_saved_filter_groups(name, status)
    end subroutine cmd_ibuffer_delete_saved_filter_groups

    module procedure ibuffer_switch_to_saved_filter_groups
    type(ibuffer_filter_groups_type) :: groups
    integer :: k, i

    i = ibuffer_saved_filter_groups%find(name)

    if (i > 0) then
        groups = ibuffer_saved_filter_groups%entries(i)%groups
    else
        allocate (groups%groups(0))
    end if

    call ibuffer_set_filter_groups(groups)
    k = ibuffer_local_index(.true.)
    deallocate (locals(k)%hidden_groups)
    allocate (locals(k)%hidden_groups(0))
    call ibuffer_update(status, .false., .true.)
    end procedure ibuffer_switch_to_saved_filter_groups

    recursive subroutine cmd_ibuffer_switch_to_saved_filter_groups(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        if (ibuffer_saved_filter_groups%count() == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No saved filters")
            return
        else if (ibuffer_saved_filter_groups%count() == 1) then
            ! `ibuffer_saved_filter_groups' is a user variable that defaults
            ! to empty.  We assume that with one element in this list the
            ! user knows what she wants.  See bug#12331.
            name = ibuffer_saved_filter_groups%entries(1)%name
        else
            call completing_read("Switch to saved filter group: ", names_table(saved_group_names()), name, status, &
                                 require_match=REQUIRE_MATCH_YES)
            if (.not. status%ok()) return
        end if

        call ibuffer_switch_to_saved_filter_groups(name, status)
    end subroutine cmd_ibuffer_switch_to_saved_filter_groups

    ! ------------------------------------------------------------------
    ! Filtering commands (define-ibuffer-filter)
    ! ------------------------------------------------------------------

    ! The description of the filter NAME.  Not a port.
    function filter_description(name) result(desc)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: desc
        integer :: i

        desc = ""
        i = filter_index(name)
        if (i > 0) desc = trim(ibuffer_filtering_table(i)%description)
    end function filter_description

    ! Push FILTER, a filter by NAME, and tell (with the qualifier text QSTR
    ! if HAS_QSTR).  Not a port.
    recursive subroutine apply_filter(name, filter, has_qstr, qstr, status)
        character(len=*), intent(in) :: name, qstr
        type(ibuffer_filter_type), intent(in) :: filter
        logical, intent(in) :: has_qstr
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: desc

        status = status_type()
        desc = filter_description(name)

        if (.not. ibuffer_push_filter(filter)) then
            if (has_qstr) then
                call message("Filter by "//desc//" already applied:  "//qstr)
            else
                call message("Filter by "//desc//" already applied")
            end if
        else
            if (has_qstr) then
                call message("Filter by "//desc//" added:  "//qstr)
            else
                call message("Filter by "//desc//" added")
            end if

            call ibuffer_update(status, .false., .true.)
        end if
    end subroutine apply_filter

    module procedure ibuffer_filter_by
    character(len=32) :: buf

    if (present(qualifier)) then
        call apply_filter(name, ibuffer_filter(name, qualifier=qualifier), .true., qualifier, status)
    else if (present(number)) then
        write (buf, "(i0)") number
        call apply_filter(name, ibuffer_filter(name, number=number), .true., trim(buf), status)
    else
        call apply_filter(name, ibuffer_filter(name), .false., "", status)
    end if
    end procedure ibuffer_filter_by

    ! The command of a filter with ACCEPT-LIST: the filter NAME with each
    ! of the qualifiers QUALIFIER, composed with `or'.  Not a port.
    recursive subroutine filter_by_list(name, qualifier, status)
        character(len=*), intent(in) :: name, qualifier(:)
        type(status_type), intent(out) :: status
        character(len=len(qualifier)), allocatable :: q(:)
        character(len=len(qualifier)) :: tmp
        type(ibuffer_filter_type), allocatable :: parts(:)
        type(ibuffer_filter_type) :: filter
        character(len=:), allocatable :: qstr
        integer :: j, i, n

        ! Reject equivalent filters: (or f1 f2) is same as (or f2 f1).
        allocate (q(size(qualifier)))
        n = 0

        do j = 1, size(qualifier)
            if (any(q(1:n) == qualifier(j))) cycle
            n = n + 1
            q(n) = qualifier(j)
        end do

        do j = 2, n
            tmp = q(j)
            i = j - 1

            do while (i >= 1)
                if (.not. llt(tmp, q(i))) exit
                q(i + 1) = q(i)
                i = i - 1
            end do

            q(i + 1) = tmp
        end do

        if (n == 0) then
            call apply_filter(name, ibuffer_filter(name), .true., "", status)
            return
        end if

        qstr = trim(q(1))

        do j = 2, n
            qstr = qstr//","//trim(q(j))
        end do

        if (n == 1) then
            filter = ibuffer_filter(name, qualifier=trim(q(1)))
        else
            ! Compose individual filters with `or'.
            allocate (parts(n))

            do j = 1, n
                parts(j) = ibuffer_filter(name, qualifier=trim(q(j)))
            end do

            filter = compound(IBUFFER_FILTER_OR, parts)
        end if

        call apply_filter(name, filter, .true., qstr, status)
    end subroutine filter_by_list

    module procedure ibuffer_filter_by_mode
    call filter_by_list("mode", qualifier, status)
    end procedure ibuffer_filter_by_mode

    module procedure ibuffer_filter_by_used_mode
    call filter_by_list("used_mode", qualifier, status)
    end procedure ibuffer_filter_by_used_mode

    module procedure ibuffer_filter_by_derived_mode
    call filter_by_list("derived_mode", qualifier, status)
    end procedure ibuffer_filter_by_derived_mode

    module procedure ibuffer_filter_by_predicate
    ! (The qualifier, a procedure, has no printed form.)
    call apply_filter("predicate", ibuffer_predicate(qualifier), .false., "", status)
    end procedure ibuffer_filter_by_predicate

    module procedure ibuffer_list_buffer_modes
    type(handle_type), allocatable :: bufs(:)
    character(len=IBUFFER_NAME_LEN), allocatable :: found(:), grown(:)
    character(len=:), allocatable :: name
    type(major_mode_type), pointer :: p
    logical :: parents
    integer :: j, n

    parents = .false.
    if (present(include_parents)) parents = include_parents
    bufs = buffer_list()
    allocate (found(16))
    n = 0

    do j = 1, size(bufs)
        name = major_mode_of(bufs(j))
        p => major_mode_named(name)

        do
            if (any(found(1:n) == name)) exit

            if (n == size(found)) then
                allocate (grown(2*n))
                grown(1:n) = found
                call move_alloc(grown, found)
            end if

            n = n + 1
            found(n) = name
            if (.not. parents) exit
            if (.not. associated(p)) exit
            p => p%parent
            if (.not. associated(p)) exit
            name = trim(p%name)
        end do
    end do

    ! (Pushed: the last found first.)
    modes = found(n:1:-1)
    end procedure ibuffer_list_buffer_modes

    ! PROMPT with DEFAULT, as format-prompt makes it.  Not a port.
    function prompt_with_default(prompt, default) result(full)
        character(len=*), intent(in) :: prompt, default
        character(len=:), allocatable :: full

        full = prompt
        if (len(default) > 0) full = full//" (default "//default//")"
        full = full//": "
    end function prompt_with_default

    ! Whether ELT is a symbol whose name ends in "_mode" (string-match
    ! "-mode\\'").  Not a port.
    logical function is_mode_name(elt)
        type(value_type), intent(in) :: elt
        integer :: n

        is_mode_name = .false.
        if ((vtag(elt) /= V_NAME .and. vtag(elt) /= V_NULL)) return
        n = len(symbols(xsymbol(elt))%name)
        if (n < 5) return
        is_mode_name = symbols(xsymbol(elt))%name(n - 4:n) == "_mode"
    end function is_mode_name

    ! Read a comma separated list of mode names with PROMPT from TABLE
    ! (with PREDICATE if MODES_ONLY), offering DEFAULT.  Not a port.
    recursive subroutine read_modes(prompt, table, default, modes_only, modes, status)
        character(len=*), intent(in) :: prompt, default
        type(completion_table_type), intent(in) :: table
        logical, intent(in) :: modes_only
        character(len=IBUFFER_NAME_LEN), allocatable, intent(out) :: modes(:)
        type(status_type), intent(out) :: status
        type(minibuffer_string_type), allocatable :: results(:)
        integer :: j

        if (modes_only .and. len(default) > 0) then
            call completing_read_multiple(prompt, table, results, status, predicate=is_mode_name, &
                                          require_match=REQUIRE_MATCH_YES, default=default)
        else if (modes_only) then
            call completing_read_multiple(prompt, table, results, status, predicate=is_mode_name, &
                                          require_match=REQUIRE_MATCH_YES)
        else if (len(default) > 0) then
            call completing_read_multiple(prompt, table, results, status, require_match=REQUIRE_MATCH_YES, &
                                          default=default)
        else
            call completing_read_multiple(prompt, table, results, status, require_match=REQUIRE_MATCH_YES)
        end if

        if (.not. status%ok()) return
        allocate (modes(size(results)))

        do j = 1, size(results)
            modes(j) = results(j)%s
        end do
    end subroutine read_modes

    ! The major mode of the buffer on this line ("" for none).  Not a port.
    function line_buffer_mode() result(default)
        character(len=:), allocatable :: default
        type(handle_type) :: buf
        type(status_type) :: st

        default = ""
        buf = ibuffer_current_buffer(st)

        if (buf%index /= 0) then
            if (is_buffer_live(buf)) default = major_mode_of(buf)
        end if
    end function line_buffer_mode

    ! A completion table of the mode names MODES.  Not a port.
    function modes_table(modes) result(table)
        character(len=*), intent(in) :: modes(:)
        type(completion_table_type) :: table
        type(minibuffer_string_type), allocatable :: names(:)
        integer :: j

        allocate (names(size(modes)))

        do j = 1, size(modes)
            names(j)%s = trim(modes(j))
        end do

        table = names_table(names)
    end function modes_table

    recursive subroutine cmd_ibuffer_filter_by_mode(status)
        type(status_type), intent(out) :: status
        character(len=IBUFFER_NAME_LEN), allocatable :: modes(:)
        type(completion_table_type) :: table
        character(len=:), allocatable :: default

        default = line_buffer_mode()
        table%kind = TABLE_OBARRAY
        call read_modes(prompt_with_default("Filter by major mode", default), table, default, .true., modes, status)
        if (status%ok()) call ibuffer_filter_by_mode(modes, status)
    end subroutine cmd_ibuffer_filter_by_mode

    recursive subroutine cmd_ibuffer_filter_by_used_mode(status)
        type(status_type), intent(out) :: status
        character(len=IBUFFER_NAME_LEN), allocatable :: modes(:), in_use(:)
        character(len=:), allocatable :: default

        default = line_buffer_mode()
        call ibuffer_list_buffer_modes(in_use)
        call read_modes(prompt_with_default("Filter by major mode", default), modes_table(in_use), default, .false., &
                        modes, status)
        if (status%ok()) call ibuffer_filter_by_used_mode(modes, status)
    end subroutine cmd_ibuffer_filter_by_used_mode

    recursive subroutine cmd_ibuffer_filter_by_derived_mode(status)
        type(status_type), intent(out) :: status
        character(len=IBUFFER_NAME_LEN), allocatable :: modes(:), in_use(:)

        call ibuffer_list_buffer_modes(in_use, .true.)
        call read_modes("Filter by derived mode: ", modes_table(in_use), "", .false., modes, status)
        if (status%ok()) call ibuffer_filter_by_derived_mode(modes, status)
    end subroutine cmd_ibuffer_filter_by_derived_mode

    recursive subroutine cmd_ibuffer_filter_by_name(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s

        call read_from_minibuffer("Filter by name (regexp): ", s, status)
        if (status%ok()) call ibuffer_filter_by_name(s, status)
    end subroutine cmd_ibuffer_filter_by_name

    recursive subroutine cmd_ibuffer_filter_by_process(status)
        type(status_type), intent(out) :: status

        call ibuffer_filter_by_process(status)
    end subroutine cmd_ibuffer_filter_by_process

    recursive subroutine cmd_ibuffer_filter_by_starred_name(status)
        type(status_type), intent(out) :: status

        call ibuffer_filter_by_starred_name(status)
    end subroutine cmd_ibuffer_filter_by_starred_name

    recursive subroutine cmd_ibuffer_filter_by_filename(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s

        call read_from_minibuffer("Filter by full file name (regexp): ", s, status)
        if (status%ok()) call ibuffer_filter_by_filename(s, status)
    end subroutine cmd_ibuffer_filter_by_filename

    recursive subroutine cmd_ibuffer_filter_by_basename(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s

        call read_from_minibuffer("Filter by file name, without directory part (regex): ", s, status)
        if (status%ok()) call ibuffer_filter_by_basename(s, status)
    end subroutine cmd_ibuffer_filter_by_basename

    recursive subroutine cmd_ibuffer_filter_by_file_extension(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s

        call read_from_minibuffer("Filter by filename extension without separator (regex): ", s, status)
        if (status%ok()) call ibuffer_filter_by_file_extension(s, status)
    end subroutine cmd_ibuffer_filter_by_file_extension

    recursive subroutine cmd_ibuffer_filter_by_directory(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s

        call read_from_minibuffer("Filter by directory name (regex): ", s, status)
        if (status%ok()) call ibuffer_filter_by_directory(s, status)
    end subroutine cmd_ibuffer_filter_by_directory

    ! (string-to-number S) for a size: its integer part (0 if S is not a
    ! number).  Not a port.
    integer(int64) function size_number(s) result(n)
        character(len=*), intent(in) :: s
        integer :: k
        logical :: negative

        n = 0
        k = 1

        do while (k <= len(s))
            if (s(k:k) /= " " .and. s(k:k) /= achar(9)) exit
            k = k + 1
        end do

        negative = .false.

        if (k <= len(s)) then
            if (s(k:k) == "-" .or. s(k:k) == "+") then
                negative = s(k:k) == "-"
                k = k + 1
            end if
        end if

        do while (k <= len(s))
            if (s(k:k) < "0" .or. s(k:k) > "9") exit
            n = 10*n + (iachar(s(k:k)) - iachar("0"))
            k = k + 1
        end do

        if (negative) n = -n
    end function size_number

    recursive subroutine cmd_ibuffer_filter_by_size_gt(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s

        call read_from_minibuffer("Filter by size greater than: ", s, status)
        if (status%ok()) call ibuffer_filter_by_size_gt(size_number(s), status)
    end subroutine cmd_ibuffer_filter_by_size_gt

    recursive subroutine cmd_ibuffer_filter_by_size_lt(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s

        call read_from_minibuffer("Filter by size less than: ", s, status)
        if (status%ok()) call ibuffer_filter_by_size_lt(size_number(s), status)
    end subroutine cmd_ibuffer_filter_by_size_lt

    recursive subroutine cmd_ibuffer_filter_by_modified(status)
        type(status_type), intent(out) :: status

        call ibuffer_filter_by_modified(status)
    end subroutine cmd_ibuffer_filter_by_modified

    recursive subroutine cmd_ibuffer_filter_by_visiting_file(status)
        type(status_type), intent(out) :: status

        call ibuffer_filter_by_visiting_file(status)
    end subroutine cmd_ibuffer_filter_by_visiting_file

    recursive subroutine cmd_ibuffer_filter_by_content(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s

        call read_from_minibuffer("Filter by content (regexp): ", s, status)
        if (status%ok()) call ibuffer_filter_by_content(s, status)
    end subroutine cmd_ibuffer_filter_by_content

    !> Select and apply filter chosen by completion against available
    !> filters.  Indicates corresponding key sequences in echo area after
    !> filtering.
    !>
    !> The completion matches against the filter description text of
    !> each filter in `ibuffer_filtering_table'.  Port of
    !> ibuf-ext.el:ibuffer-filter-chosen-by-completion.
    recursive subroutine ibuffer_filter_chosen_by_completion(status)
        type(status_type), intent(out) :: status
        type(minibuffer_string_type), allocatable :: names(:)
        type(value_type), allocatable :: events(:)
        type(value_type) :: keys
        character(len=:), allocatable :: match, command
        character(len=32) :: buf
        integer :: j, i

        allocate (names(size(ibuffer_filtering_table)))

        do j = 1, size(names)
            names(j)%s = trim(ibuffer_filtering_table(j)%description)
        end do

        call completing_read("Filter by: ", names_table(names), match, status, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        i = 0

        do j = 1, size(names)
            if (names(j)%s == match) then
                i = j
                exit
            end if
        end do

        if (i == 0) then
            command = "ibuffer_filter_by_"
        else
            command = "ibuffer_filter_by_"//trim(ibuffer_filtering_table(i)%name)
        end if

        call execute_interactively(value_type(name=command), status)
        if (.not. status%ok()) return
        ! (mapconcat #'key-description KEY "or "): the first key sequence
        ! is a list of events, each given to key-description, which wants
        ! a sequence.
        call where_is_internal(value_type(name=command), keys, .true., ibuffer_mode_map)

        if (allocated(keys%a)) then
            call list_elements(keys, events)

            if (size(events) > 0) then
                if (vtag(events(1)) == V_INTEGER) then
                    write (buf, "(i0)") xfixnum(events(1))
                    call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected array or character(len=*), got "//trim(buf))
                else
                    call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected array or character(len=*)")
                end if

                return
            end if
        end if

        call message(command//" can be run with key sequences: ")
    end subroutine ibuffer_filter_chosen_by_completion

    !> Enable or disable filtering by the major mode at point.  Port of
    !> ibuf-ext.el:ibuffer-interactive-filter-by-mode.
    recursive subroutine ibuffer_interactive_filter_by_mode(event_or_point, status)
        integer(int64), intent(in) :: event_or_point
        type(status_type), intent(out) :: status
        type(ibuffer_filter_type), allocatable :: filters(:), kept(:)
        type(handle_type) :: buf
        type(status_type) :: st
        logical :: pushed
        integer :: j, n

        call goto_char(event_or_point)
        buf = ibuffer_current_buffer(st)
        filters = ibuffer_filtering_qualifiers()

        if (any([(filter_symbol(filters(j)) == "mode", j=1, size(filters))])) then
            allocate (kept(size(filters)))
            n = 0

            do j = 1, size(filters)
                if (filter_symbol(filters(j)) == "mode") cycle
                n = n + 1
                kept(n) = filters(j)
            end do

            call ibuffer_set_filtering_qualifiers(kept(1:n))
        else
            if (buf%index == 0) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(buffer_type)")
                return
            end if

            pushed = ibuffer_push_filter(ibuffer_major_mode(major_mode_of(buf)))
        end if

        call ibuffer_update(status, .false., .true.)
    end subroutine ibuffer_interactive_filter_by_mode

    recursive subroutine cmd_ibuffer_interactive_filter_by_mode(status)
        type(status_type), intent(out) :: status

        call ibuffer_interactive_filter_by_mode(point(), status)
    end subroutine cmd_ibuffer_interactive_filter_by_mode

    !> Emulate `bs_show' from the bs.el package.  Port of
    !> ibuf-ext.el:ibuffer-bs-show.
    recursive subroutine ibuffer_bs_show(status)
        type(status_type), intent(out) :: status
        type(ibuffer_filter_type) :: files(1)
        type(status_type) :: st

        files(1) = ibuffer_filename(".*")
        call ibuffer(status, other_window_p=.true., name="*Ibuffer-bs*", qualifiers=files, shrink=IBUFFER_SHRINK_YES)
        if (.not. status%ok()) return
        call keymap_set(current_local_map(), "a", "ibuffer_bs_toggle_all", st)
    end subroutine ibuffer_bs_show

    !> Emulate `bs_toggle_show_all' from the bs.el package.  Port of
    !> ibuf-ext.el:ibuffer-bs-toggle-all.
    recursive subroutine ibuffer_bs_toggle_all(status)
        type(status_type), intent(out) :: status
        logical :: pushed
        integer :: k

        k = ibuffer_local_index(.true.)

        if (size(locals(k)%qualifiers) > 0) then
            call ibuffer_pop_filter(status)
        else
            pushed = ibuffer_push_filter(ibuffer_filename(".*"))
            call ibuffer_update(status, .false., .true.)
        end if
    end subroutine ibuffer_bs_toggle_all

    module procedure syms_of_ibuf_ext
    type(status_type) :: st
    type(handle_type) :: m

    call define_ibuffer_sorter("major_mode", "major mode", sort_major_mode)
    call define_ibuffer_sorter("mode_name", "major mode name", sort_mode_name)
    call define_ibuffer_sorter("alphabetic", "buffer name", sort_alphabetic)
    call define_ibuffer_sorter("size", "size", sort_size)
    call define_ibuffer_sorter("filename/process", "file name", sort_filename_process)
    call define_ibuffer_filter("mode", "major mode", filter_mode)
    call define_ibuffer_filter("used_mode", "major mode in use", filter_mode)
    call define_ibuffer_filter("derived_mode", "derived mode", filter_derived_mode)
    call define_ibuffer_filter("name", "buffer name", filter_name)
    call define_ibuffer_filter("process", "process", filter_process)
    call define_ibuffer_filter("starred_name", "starred buffer name", filter_starred_name)
    call define_ibuffer_filter("filename", "full file name", filter_filename)
    call define_ibuffer_filter("basename", "file basename", filter_basename)
    call define_ibuffer_filter("file_extension", "filename extension", filter_file_extension)
    call define_ibuffer_filter("directory", "directory name", filter_directory)
    call define_ibuffer_filter("size_gt", "size greater than", filter_size_gt)
    call define_ibuffer_filter("size_lt", "size less than", filter_size_lt)
    call define_ibuffer_filter("modified", "modified", filter_modified)
    call define_ibuffer_filter("visiting_file", "visiting a file", filter_visiting_file)
    call define_ibuffer_filter("content", "content", filter_content)
    call define_ibuffer_filter("predicate", "predicate", filter_predicate)
    commands%ibuffer_filter_by_mode%execute => cmd_ibuffer_filter_by_mode
    commands%ibuffer_filter_by_used_mode%execute => cmd_ibuffer_filter_by_used_mode
    commands%ibuffer_filter_by_derived_mode%execute => cmd_ibuffer_filter_by_derived_mode
    commands%ibuffer_filter_by_name%execute => cmd_ibuffer_filter_by_name
    commands%ibuffer_filter_by_process%execute => cmd_ibuffer_filter_by_process
    commands%ibuffer_filter_by_starred_name%execute => cmd_ibuffer_filter_by_starred_name
    commands%ibuffer_filter_by_filename%execute => cmd_ibuffer_filter_by_filename
    commands%ibuffer_filter_by_basename%execute => cmd_ibuffer_filter_by_basename
    commands%ibuffer_filter_by_file_extension%execute => cmd_ibuffer_filter_by_file_extension
    commands%ibuffer_filter_by_directory%execute => cmd_ibuffer_filter_by_directory
    commands%ibuffer_filter_by_size_gt%execute => cmd_ibuffer_filter_by_size_gt
    commands%ibuffer_filter_by_size_lt%execute => cmd_ibuffer_filter_by_size_lt
    commands%ibuffer_filter_by_modified%execute => cmd_ibuffer_filter_by_modified
    commands%ibuffer_filter_by_visiting_file%execute => cmd_ibuffer_filter_by_visiting_file
    commands%ibuffer_filter_by_content%execute => cmd_ibuffer_filter_by_content
    commands%ibuffer_filter_chosen_by_completion%execute => ibuffer_filter_chosen_by_completion
    commands%ibuffer_interactive_filter_by_mode%execute => cmd_ibuffer_interactive_filter_by_mode
    commands%ibuffer_filter_disable%execute => cmd_ibuffer_filter_disable
    commands%ibuffer_pop_filter%execute => ibuffer_pop_filter
    commands%ibuffer_decompose_filter%execute => cmd_ibuffer_decompose_filter
    commands%ibuffer_exchange_filters%execute => cmd_ibuffer_exchange_filters
    commands%ibuffer_negate_filter%execute => cmd_ibuffer_negate_filter
    commands%ibuffer_or_filter%execute => cmd_ibuffer_or_filter
    commands%ibuffer_and_filter%execute => cmd_ibuffer_and_filter
    commands%ibuffer_save_filters%execute => cmd_ibuffer_save_filters
    commands%ibuffer_delete_saved_filters%execute => cmd_ibuffer_delete_saved_filters
    commands%ibuffer_add_saved_filters%execute => cmd_ibuffer_add_saved_filters
    commands%ibuffer_switch_to_saved_filters%execute => cmd_ibuffer_switch_to_saved_filters
    commands%ibuffer_filters_to_filter_group%execute => cmd_ibuffer_filters_to_filter_group
    commands%ibuffer_set_filter_groups_by_mode%execute => cmd_ibuffer_set_filter_groups_by_mode
    commands%ibuffer_pop_filter_group%execute => cmd_ibuffer_pop_filter_group
    commands%ibuffer_decompose_filter_group%execute => cmd_ibuffer_decompose_filter_group
    commands%ibuffer_clear_filter_groups%execute => cmd_ibuffer_clear_filter_groups
    commands%ibuffer_kill_filter_group%execute => cmd_ibuffer_kill_filter_group
    commands%ibuffer_kill_line%execute => ibuffer_kill_line
    commands%ibuffer_yank%execute => ibuffer_yank
    commands%ibuffer_yank_filter_group%execute => cmd_ibuffer_yank_filter_group
    commands%ibuffer_save_filter_groups%execute => cmd_ibuffer_save_filter_groups
    commands%ibuffer_delete_saved_filter_groups%execute => cmd_ibuffer_delete_saved_filter_groups
    commands%ibuffer_switch_to_saved_filter_groups%execute => cmd_ibuffer_switch_to_saved_filter_groups
    commands%ibuffer_bs_show%execute => ibuffer_bs_show
    commands%ibuffer_bs_toggle_all%execute => ibuffer_bs_toggle_all
    call minor_modes%ibuffer_auto_mode%define("ibuffer_auto_mode", body=ibuffer_auto_mode)
    commands%ibuffer_auto_mode%execute => cmd_ibuffer_auto_mode
    commands%ibuffer_toggle_filter_group%execute => ibuffer_toggle_filter_group
    commands%ibuffer_forward_filter_group%execute => cmd_ibuffer_forward_filter_group
    commands%ibuffer_backward_filter_group%execute => cmd_ibuffer_backward_filter_group
    commands%ibuffer_jump_to_filter_group%execute => cmd_ibuffer_jump_to_filter_group
    commands%ibuffer_do_shell_command_pipe%execute => cmd_ibuffer_do_shell_command_pipe
    commands%ibuffer_do_shell_command_pipe_replace%execute => cmd_ibuffer_do_shell_command_pipe_replace
    commands%ibuffer_do_shell_command_file%execute => cmd_ibuffer_do_shell_command_file
    commands%ibuffer_do_rename_uniquely%execute => ibuffer_do_rename_uniquely
    commands%ibuffer_do_revert%execute => ibuffer_do_revert
    commands%ibuffer_do_replace_regexp%execute => cmd_ibuffer_do_replace_regexp
    commands%ibuffer_do_query_replace%execute => cmd_ibuffer_do_query_replace
    commands%ibuffer_do_query_replace_regexp%execute => cmd_ibuffer_do_query_replace_regexp
    commands%ibuffer_toggle_sorting_mode%execute => ibuffer_toggle_sorting_mode
    commands%ibuffer_invert_sorting%execute => ibuffer_invert_sorting
    commands%ibuffer_do_sort_by_major_mode%execute => ibuffer_do_sort_by_major_mode
    commands%ibuffer_do_sort_by_mode_name%execute => ibuffer_do_sort_by_mode_name
    commands%ibuffer_do_sort_by_alphabetic%execute => ibuffer_do_sort_by_alphabetic
    commands%ibuffer_do_sort_by_size%execute => ibuffer_do_sort_by_size
    commands%ibuffer_do_sort_by_filename_process%execute => ibuffer_do_sort_by_filename_process
    commands%ibuffer_add_to_tmp_hide%execute => cmd_ibuffer_add_to_tmp_hide
    commands%ibuffer_add_to_tmp_show%execute => cmd_ibuffer_add_to_tmp_show
    commands%ibuffer_forward_next_marked%execute => cmd_ibuffer_forward_next_marked
    commands%ibuffer_backwards_next_marked%execute => cmd_ibuffer_backwards_next_marked
    commands%ibuffer_do_kill_lines%execute => ibuffer_do_kill_lines
    commands%ibuffer_jump_to_buffer%execute => cmd_ibuffer_jump_to_buffer
    commands%ibuffer_diff_with_file%execute => ibuffer_diff_with_file
    commands%ibuffer_copy_filename_as_kill%execute => ibuffer_copy_filename_as_kill
    commands%ibuffer_copy_buffername_as_kill%execute => ibuffer_copy_buffername_as_kill
    commands%ibuffer_mark_by_name_regexp%execute => cmd_ibuffer_mark_by_name_regexp
    commands%ibuffer_mark_by_locked%execute => ibuffer_mark_by_locked
    commands%ibuffer_mark_by_mode_regexp%execute => cmd_ibuffer_mark_by_mode_regexp
    commands%ibuffer_mark_by_file_name_regexp%execute => cmd_ibuffer_mark_by_file_name_regexp
    commands%ibuffer_mark_by_content_regexp%execute => cmd_ibuffer_mark_by_content_regexp
    commands%ibuffer_mark_by_mode%execute => cmd_ibuffer_mark_by_mode
    commands%ibuffer_mark_modified_buffers%execute => ibuffer_mark_modified_buffers
    commands%ibuffer_mark_unsaved_buffers%execute => ibuffer_mark_unsaved_buffers
    commands%ibuffer_mark_dissociated_buffers%execute => ibuffer_mark_dissociated_buffers
    commands%ibuffer_mark_help_buffers%execute => ibuffer_mark_help_buffers
    commands%ibuffer_mark_compressed_file_buffers%execute => ibuffer_mark_compressed_file_buffers
    commands%ibuffer_mark_old_buffers%execute => ibuffer_mark_old_buffers
    commands%ibuffer_mark_special_buffers%execute => ibuffer_mark_special_buffers
    commands%ibuffer_mark_read_only_buffers%execute => ibuffer_mark_read_only_buffers
    commands%ibuffer_mark_dired_buffers%execute => ibuffer_mark_dired_buffers
    commands%ibuffer_do_occur%execute => cmd_ibuffer_do_occur
    m = ibuffer_mode_map
    call keymap_set(m, "=", "ibuffer_diff_with_file", st)
    call keymap_set(m, "j", "ibuffer_jump_to_buffer", st)
    call keymap_set(m, "M-g", "ibuffer_jump_to_buffer", st)
    call keymap_set(m, "M-s a C-o", "ibuffer_do_occur", st)
    call keymap_set(m, "* M", "ibuffer_mark_by_mode", st)
    call keymap_set(m, "* m", "ibuffer_mark_modified_buffers", st)
    call keymap_set(m, "* u", "ibuffer_mark_unsaved_buffers", st)
    call keymap_set(m, "* s", "ibuffer_mark_special_buffers", st)
    call keymap_set(m, "* r", "ibuffer_mark_read_only_buffers", st)
    call keymap_set(m, "* /", "ibuffer_mark_dired_buffers", st)
    call keymap_set(m, "* e", "ibuffer_mark_dissociated_buffers", st)
    call keymap_set(m, "* h", "ibuffer_mark_help_buffers", st)
    call keymap_set(m, "* z", "ibuffer_mark_compressed_file_buffers", st)
    call keymap_set(m, ".", "ibuffer_mark_old_buffers", st)
    call keymap_set(m, "M-}", "ibuffer_forward_next_marked", st)
    call keymap_set(m, "M-{", "ibuffer_backwards_next_marked", st)
    call keymap_set(m, "-", "ibuffer_add_to_tmp_hide", st)
    call keymap_set(m, "+", "ibuffer_add_to_tmp_show", st)
    call keymap_set(m, ",", "ibuffer_toggle_sorting_mode", st)
    call keymap_set(m, "s i", "ibuffer_invert_sorting", st)
    call keymap_set(m, "s a", "ibuffer_do_sort_by_alphabetic", st)
    call keymap_set(m, "s s", "ibuffer_do_sort_by_size", st)
    call keymap_set(m, "s f", "ibuffer_do_sort_by_filename_process", st)
    call keymap_set(m, "s m", "ibuffer_do_sort_by_major_mode", st)
    call keymap_set(m, "M-n", "ibuffer_forward_filter_group", st)
    call keymap_set(m, "TAB", "ibuffer_forward_filter_group", st)
    call keymap_set(m, "M-p", "ibuffer_backward_filter_group", st)
    call keymap_set(m, "<backtab>", "ibuffer_backward_filter_group", st)
    call keymap_set(m, "M-j", "ibuffer_jump_to_filter_group", st)
    call keymap_set(m, "C-k", "ibuffer_kill_line", st)
    call keymap_set(m, "C-y", "ibuffer_yank", st)
    ! (ibuffer--filter-map)
    call keymap_set(m, "/ RET", "ibuffer_filter_by_mode", st)
    call keymap_set(m, "/ SPC", "ibuffer_filter_chosen_by_completion", st)
    call keymap_set(m, "/ m", "ibuffer_filter_by_used_mode", st)
    call keymap_set(m, "/ M", "ibuffer_filter_by_derived_mode", st)
    call keymap_set(m, "/ n", "ibuffer_filter_by_name", st)
    call keymap_set(m, "/ E", "ibuffer_filter_by_process", st)
    call keymap_set(m, "/ *", "ibuffer_filter_by_starred_name", st)
    call keymap_set(m, "/ f", "ibuffer_filter_by_filename", st)
    call keymap_set(m, "/ F", "ibuffer_filter_by_directory", st)
    call keymap_set(m, "/ b", "ibuffer_filter_by_basename", st)
    call keymap_set(m, "/ .", "ibuffer_filter_by_file_extension", st)
    call keymap_set(m, "/ <", "ibuffer_filter_by_size_lt", st)
    call keymap_set(m, "/ >", "ibuffer_filter_by_size_gt", st)
    call keymap_set(m, "/ i", "ibuffer_filter_by_modified", st)
    call keymap_set(m, "/ v", "ibuffer_filter_by_visiting_file", st)
    call keymap_set(m, "/ c", "ibuffer_filter_by_content", st)
    call keymap_set(m, "/ r", "ibuffer_switch_to_saved_filters", st)
    call keymap_set(m, "/ a", "ibuffer_add_saved_filters", st)
    call keymap_set(m, "/ x", "ibuffer_delete_saved_filters", st)
    call keymap_set(m, "/ d", "ibuffer_decompose_filter", st)
    call keymap_set(m, "/ s", "ibuffer_save_filters", st)
    call keymap_set(m, "/ p", "ibuffer_pop_filter", st)
    call keymap_set(m, "/ <up>", "ibuffer_pop_filter", st)
    call keymap_set(m, "/ !", "ibuffer_negate_filter", st)
    call keymap_set(m, "/ t", "ibuffer_exchange_filters", st)
    call keymap_set(m, "/ TAB", "ibuffer_exchange_filters", st)
    call keymap_set(m, "/ o", "ibuffer_or_filter", st)
    call keymap_set(m, "/ |", "ibuffer_or_filter", st)
    call keymap_set(m, "/ &", "ibuffer_and_filter", st)
    call keymap_set(m, "/ g", "ibuffer_filters_to_filter_group", st)
    call keymap_set(m, "/ P", "ibuffer_pop_filter_group", st)
    call keymap_set(m, "/ S-<up>", "ibuffer_pop_filter_group", st)
    call keymap_set(m, "/ D", "ibuffer_decompose_filter_group", st)
    call keymap_set(m, "/ /", "ibuffer_filter_disable", st)
    call keymap_set(m, "/ S", "ibuffer_save_filter_groups", st)
    call keymap_set(m, "/ R", "ibuffer_switch_to_saved_filter_groups", st)
    call keymap_set(m, "/ X", "ibuffer_delete_saved_filter_groups", st)
    call keymap_set(m, "/ \", "ibuffer_clear_filter_groups", st)
    call keymap_set(m, "% n", "ibuffer_mark_by_name_regexp", st)
    call keymap_set(m, "% m", "ibuffer_mark_by_mode_regexp", st)
    call keymap_set(m, "% f", "ibuffer_mark_by_file_name_regexp", st)
    call keymap_set(m, "% g", "ibuffer_mark_by_content_regexp", st)
    call keymap_set(m, "% L", "ibuffer_mark_by_locked", st)
    call keymap_set(m, "|", "ibuffer_do_shell_command_pipe", st)
    call keymap_set(m, "!", "ibuffer_do_shell_command_file", st)
    call keymap_set(m, "F", "ibuffer_do_shell_command_file", st)
    call keymap_set(m, "I", "ibuffer_do_query_replace_regexp", st)
    call keymap_set(m, "N", "ibuffer_do_shell_command_pipe_replace", st)
    call keymap_set(m, "O", "ibuffer_do_occur", st)
    call keymap_set(m, "Q", "ibuffer_do_query_replace", st)
    call keymap_set(m, "R", "ibuffer_do_rename_uniquely", st)
    call keymap_set(m, "r", "ibuffer_do_replace_regexp", st)
    call keymap_set(m, "V", "ibuffer_do_revert", st)
    call keymap_set(m, "X", "ibuffer_do_shell_command_pipe", st)
    call keymap_set(m, "k", "ibuffer_do_kill_lines", st)
    call keymap_set(m, "w", "ibuffer_copy_filename_as_kill", st)
    call keymap_set(m, "B", "ibuffer_copy_buffername_as_kill", st)
    call keymap_set(m, "C-c C-a", "ibuffer_auto_mode", st)
    ! (ibuffer-mode-filter-group-map)
    call make_filter_group_map()
    end procedure syms_of_ibuf_ext

    ! Not a port.
    subroutine make_filter_group_map()
        use keymap_fm, only: make_sparse_keymap, is_keymap_live, free_keymap
        type(status_type) :: st

        if (is_keymap_live(ibuffer_mode_filter_group_map)) call free_keymap(ibuffer_mode_filter_group_map, st)
        call make_sparse_keymap(ibuffer_mode_filter_group_map)
        call keymap_set(ibuffer_mode_filter_group_map, "RET", "ibuffer_toggle_filter_group", st)
    end subroutine make_filter_group_map
end submodule ibuf_ext_fm
