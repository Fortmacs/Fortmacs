! textprop_fm.f90 -- text properties of buffer text.
! Ported from GNU Emacs 31.1 src/textprop.c.
!
! Copyright (C) 1993-2026 Free Software Foundation, Inc.
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

!> Text properties of the current buffer.
!>
!> Functions that return nil upstream take a `found' logical.  Changing
!> properties counts as a modification (buffer modified, read-only text
!> checked, change hooks run) as upstream.  `get_char_property' and the
!> char-property-change functions look at overlays too.  Strings keep
!> their properties as lisp_fm's string intervals: the `string_'
!> functions and `propertize' work on them (0-based positions, as
!> upstream).  Not yet: the other functions with a string OBJECT, undo
!> records, modification hooks (the `modification_hooks'
!> property and friends), LIMIT t in next-property-change,
!> add-face-text-property, stickiness for insert-and-inherit, and
!> `text_property_default_nonsticky' as a (buffer-local) variable: its
!> standard value is built in, as the value model has no dotted pairs
!> for its alist.
module textprop_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_ARGS_OUT_OF_RANGE, &
                        FM_GENERIC_ERROR, FM_TEXT_READ_ONLY, FM_BUFFER_READ_ONLY, FM_WRONG_TYPE_ARGUMENT
    use lisp_fm, only: consp, list, list_elements, list_length, make_list, make_symbol_ref, nilp, &
                       stringp, value_type, xstring, nil, string_intervals, set_string_intervals, &
                       make_string, symbolp, symbols, xsymbol
    use character_fm, only: multibyte_chars_in_text
    use lread_fm, only: intern
    use data_fm, only: value_eq
    use buffer_fm, only: buffers, cur, BEG, inhibit_read_only
    use handle_fm, only: handle_type, null_handle
    use itree_fm, only: overlays_at, overlay_get_prop, next_overlay_change, previous_overlay_change, overlays_in, &
                        overlay_start, overlay_end, overlay_advance, overlay_priority, compare_overlays
    use intervals_fm, only: create_root_interval, find_interval, interval_end, intervals_equal, &
                            plist_get_eq, split_interval_left, split_interval_right, textget, tmem
    implicit none(type, external)
    private

    public :: text_properties_at, get_text_property, put_text_property, &
              add_text_properties, set_text_properties, remove_text_properties, &
              remove_list_of_text_properties, next_property_change, &
              next_single_property_change, previous_property_change, &
              previous_single_property_change, text_property_any, &
              text_property_not_all, verify_interval_modification, add_face_text_property
    public :: text_property_stickiness, next_single_char_property_change, &
              previous_single_char_property_change, get_char_property, get_char_property_and_overlay, &
              next_char_property_change, previous_char_property_change, text_prop_means_invisible, &
              get_pos_property, get_property_and_range
    public :: string_text_properties_at, string_get_text_property, string_add_text_properties, &
              string_put_text_property, propertize, string_next_single_property_change, propertized_substring, &
              propertized_concat

    interface
        !> Check that the properties of START..END may change, run the
        !> before-change hooks, and record the modification.  Port of
        !> textprop.c:modify_text_properties.  (In textprop_modify_fm,
        !> which can use insdel_fm.)
        recursive module subroutine modify_text_properties(start, end, status)
            implicit none(type, external)
            integer(int64), intent(in) :: start, end
            type(status_type), intent(out) :: status
        end subroutine modify_text_properties

        !> Run the after-change hooks for a change of the properties of
        !> START..END (the signal_after_change calls of textprop.c).  (In
        !> textprop_modify_fm.)
        recursive module subroutine signal_text_properties_change(start, end, status)
            implicit none(type, external)
            integer(int64), intent(in) :: start, end
            type(status_type), intent(out) :: status
        end subroutine signal_text_properties_change
    end interface

contains

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    function qsym(name) result(v)
        character(len=*), intent(in) :: name
        type(value_type) :: v
        v = make_symbol_ref(intern(name))
    end function qsym

    subroutine range_error(status, a, b)
        type(status_type), intent(inout) :: status
        integer(int64), intent(in) :: a, b
        character(len=24) :: sa, sb
        write (sa, "(i0)") a
        write (sb, "(i0)") b
        call set_error(status, FM_ARGS_OUT_OF_RANGE, trim(sa)//", "//trim(sb))
    end subroutine range_error

    !> Order and check a range of the current buffer.  `nonempty' is false
    !> when there is nothing to work on.  Port of the buffer case of
    !> textprop.c:validate_interval_range.
    subroutine validate_range(start, end, s, e, nonempty, status)
        integer(int64), intent(in) :: start, end
        integer(int64), intent(out) :: s, e
        logical, intent(out) :: nonempty
        type(status_type), intent(out) :: status
        s = min(start, end)
        e = max(start, end)
        nonempty = .false.
        if (.not. (buffers(cur)%begv <= s .and. e <= buffers(cur)%zv)) then
            call range_error(status, start, end)
            return
        end if
        nonempty = s /= e .and. buffers(cur)%begv /= buffers(cur)%zv
    end subroutine validate_range

    !> Give the current buffer intervals if it has none.
    !> Port of intervals.c:create_root_interval (buffer case).
    subroutine ensure_intervals()
        associate (t => buffers(cur)%text)
            if (t%intervals%n == 0) call create_root_interval(t%intervals, BEG, t%z - BEG)
        end associate
    end subroutine ensure_intervals

    !> nil stays nil; an even-length list is kept; an odd-length list is an
    !> error; anything else X becomes (X nil).  Port of textprop.c:validate_plist.
    subroutine validate_plist(props, plist, status)
        type(value_type), intent(in) :: props
        type(value_type), intent(out) :: plist
        type(status_type), intent(out) :: status
        type(value_type) :: nil_v
        if (nilp(props)) return
        if (consp(props)) then
            if (mod(list_length(props), 2_int64) /= 0) then
                call set_error(status, FM_GENERIC_ERROR, "Odd length text property list")
                return
            end if
            plist = props
        else
            plist = list(props, nil_v)
        end if
    end subroutine validate_plist

    !> True if `x' is eq to an element of list `set'.
    logical function memq_p(x, set)
        type(value_type), intent(in) :: x, set
        type(value_type), allocatable :: elems(:)
        integer(int64) :: k
        memq_p = .false.
        call list_elements(set, elems)
        do k = 1, size(elems, kind=int64)
            if (value_eq(elems(k), x)) then
                memq_p = .true.
                return
            end if
        end do
    end function memq_p

    !> Signal text-read-only.  Port of textprop.c:text_read_only.
    subroutine text_read_only(status, propval)
        type(status_type), intent(inout) :: status
        type(value_type), intent(in) :: propval
        if (stringp(propval)) then
            call set_error(status, FM_TEXT_READ_ONLY, xstring(propval))
        else
            call set_error(status, FM_TEXT_READ_ONLY)
        end if
    end subroutine text_read_only

    ! ------------------------------------------------------------------
    ! Per-interval plist edits (textprop.c)
    ! ------------------------------------------------------------------

    !> Port of textprop.c:interval_has_all_properties.
    logical function has_all_properties(plist, k)
        type(value_type), intent(in) :: plist
        integer(int64), intent(in) :: k
        type(value_type), allocatable :: elems(:)
        type(value_type) :: val
        logical :: found
        integer(int64) :: j
        has_all_properties = .false.
        call list_elements(plist, elems)
        do j = 1, size(elems, kind=int64) - 1, 2
            call plist_get_eq(buffers(cur)%text%intervals%runs(k)%plist, elems(j), val, found)
            if (.not. found) return
            if (.not. value_eq(val, elems(j + 1))) return
        end do
        has_all_properties = .true.
    end function has_all_properties

    !> True if interval `k' has any of the properties named in `names'
    !> (every element when `step' is 1, every other one for a plist).
    !> Port of textprop.c:interval_has_some_properties(_list).
    logical function has_some_properties(names, step, k)
        type(value_type), intent(in) :: names
        integer(int64), intent(in) :: step, k
        type(value_type), allocatable :: elems(:)
        type(value_type) :: val
        logical :: found
        integer(int64) :: j
        has_some_properties = .false.
        call list_elements(names, elems)
        do j = 1, size(elems, kind=int64), step
            call plist_get_eq(buffers(cur)%text%intervals%runs(k)%plist, elems(j), val, found)
            if (found) then
                has_some_properties = .true.
                return
            end if
        end do
    end function has_some_properties

    !> Set or add each property of `plist' on interval `k'; new properties
    !> go in front, as upstream.  Port of textprop.c:add_properties
    !> (TEXT_PROPERTY_REPLACE).
    subroutine add_properties(plist, k, changed)
        type(value_type), intent(in) :: plist
        integer(int64), intent(in) :: k
        logical, intent(out) :: changed
        type(value_type), allocatable :: new(:), old(:), grown(:)
        integer(int64) :: j, m
        logical :: found
        changed = .false.
        call list_elements(plist, new)
        do j = 1, size(new, kind=int64) - 1, 2
            call list_elements(buffers(cur)%text%intervals%runs(k)%plist, old)
            found = .false.
            do m = 1, size(old, kind=int64) - 1, 2
                if (value_eq(old(m), new(j))) then
                    found = .true.
                    if (.not. value_eq(old(m + 1), new(j + 1))) then
                        old(m + 1) = new(j + 1)
                        buffers(cur)%text%intervals%runs(k)%plist = make_list(old)
                        changed = .true.
                    end if
                    exit
                end if
            end do
            if (.not. found) then
                grown = [new(j:j + 1), old]
                buffers(cur)%text%intervals%runs(k)%plist = make_list(grown)
                changed = .true.
            end if
        end do
    end subroutine add_properties

    !> Remove from interval `k' every property named in `names' (every
    !> element when `step' is 1, every other one for a plist).
    !> Port of textprop.c:remove_properties.
    subroutine remove_properties(names, step, k, changed)
        type(value_type), intent(in) :: names
        integer(int64), intent(in) :: step, k
        logical, intent(out) :: changed
        type(value_type), allocatable :: syms(:), old(:), kept(:)
        integer(int64) :: j, m, n
        changed = .false.
        call list_elements(names, syms)
        do j = 1, size(syms, kind=int64), step
            call list_elements(buffers(cur)%text%intervals%runs(k)%plist, old)
            allocate (kept(size(old)))
            n = 0
            do m = 1, size(old, kind=int64) - 1, 2
                if (value_eq(old(m), syms(j))) then
                    changed = .true.
                else
                    kept(n + 1) = old(m)
                    kept(n + 2) = old(m + 1)
                    n = n + 2
                end if
            end do
            buffers(cur)%text%intervals%runs(k)%plist = make_list(kept(1:n))
            deallocate (kept)
        end do
    end subroutine remove_properties

    ! ------------------------------------------------------------------
    ! Queries
    ! ------------------------------------------------------------------

    !> Index of the interval at `pos', or 0 if there is none (no intervals,
    !> empty accessible region, or `pos' at the end of the text).
    integer(int64) function interval_at(pos)
        integer(int64), intent(in) :: pos
        interval_at = 0
        associate (b => buffers(cur), iv => buffers(cur)%text%intervals)
            if (iv%n == 0 .or. b%begv == b%zv) return
            interval_at = find_interval(iv, pos)
            if (pos == interval_end(iv, interval_at)) interval_at = 0
        end associate
    end function interval_at

    !> Property list of the character after `position'.
    !> Port of textprop.c:Ftext_properties_at.
    subroutine text_properties_at(position, plist, status)
        integer(int64), intent(in) :: position
        type(value_type), intent(out) :: plist
        type(status_type), intent(out) :: status
        integer(int64) :: k
        if (.not. (buffers(cur)%begv <= position .and. position <= buffers(cur)%zv)) then
            call range_error(status, position, position)
            return
        end if
        k = interval_at(position)
        if (k > 0) plist = buffers(cur)%text%intervals%runs(k)%plist
    end subroutine text_properties_at

    !> Value of `prop' on the character after `position' (via `category'
    !> too).  Port of textprop.c:Fget_text_property.
    subroutine get_text_property(position, prop, val, status)
        integer(int64), intent(in) :: position
        type(value_type), intent(in) :: prop
        type(value_type), intent(out) :: val
        type(status_type), intent(out) :: status
        type(value_type) :: plist
        call text_properties_at(position, plist, status)
        if (status%ok()) val = textget(plist, prop)
    end subroutine get_text_property

    ! ------------------------------------------------------------------
    ! Changing properties
    ! ------------------------------------------------------------------

    !> Whether interval K, which was at PREV_POS with PREV_LEN characters
    !> among PREV_N intervals in a buffer of PREV_Z, is still as it was:
    !> a before-change function may have changed the intervals behind our
    !> back (the check after modify_text_properties in textprop.c).
    logical function interval_unchanged(k, prev_n, prev_pos, prev_len, prev_z) result(same)
        integer(int64), intent(in) :: k, prev_n, prev_pos, prev_len, prev_z
        same = .false.
        associate (iv => buffers(cur)%text%intervals)
            if (iv%n /= prev_n .or. buffers(cur)%text%z /= prev_z) return
            same = iv%runs(k)%position == prev_pos .and. iv%runs(k)%length == prev_len
        end associate
    end function interval_unchanged

    !> Add the properties in `properties' to the text from `start' to
    !> `end'.  `changed' tells whether anything changed.  The change hooks
    !> run.  Port of textprop.c:Fadd_text_properties / add_text_properties_1.
    recursive subroutine add_text_properties(start, end, properties, changed, status)
        integer(int64), intent(in) :: start, end
        type(value_type), intent(in) :: properties
        logical, intent(out) :: changed
        type(status_type), intent(out) :: status
        type(value_type) :: plist
        integer(int64) :: s, e, len, k, got, r, prev_n, prev_pos, prev_len, prev_z
        logical :: nonempty, any_change, first_time, done
        changed = .false.
        call validate_plist(properties, plist, status)
        if (.not. status%ok() .or. nilp(plist)) return
        first_time = .true.
        do
            call validate_range(start, end, s, e, nonempty, status)
            if (.not. status%ok() .or. .not. nonempty) return
            call ensure_intervals()
            done = .false.
            associate (iv => buffers(cur)%text%intervals)
                len = e - s
                k = find_interval(iv, s)
                ! If this interval already has the properties, skip it.
                if (has_all_properties(plist, k)) then
                    got = iv%runs(k)%length - (s - iv%runs(k)%position)
                    do
                        if (got >= len) then
                            done = .true.
                            exit
                        end if
                        len = len - got
                        k = k + 1
                        got = iv%runs(k)%length
                        if (.not. has_all_properties(plist, k)) exit
                    end do
                else if (iv%runs(k)%position /= s) then
                    call split_interval_right(iv, k, s - iv%runs(k)%position, r)
                    k = r
                end if
                if (.not. done) then
                    prev_n = iv%n
                    prev_pos = iv%runs(k)%position
                    prev_len = iv%runs(k)%length
                end if
            end associate
            if (done) return
            if (.not. first_time) exit
            prev_z = buffers(cur)%text%z
            call modify_text_properties(s, e, status)
            if (.not. status%ok()) return
            ! If the intervals changed behind our back, start over.
            first_time = .false.
            if (interval_unchanged(k, prev_n, prev_pos, prev_len, prev_z)) exit
        end do
        changed = .true.
        ! At the start of interval K, with LEN chars to go.
        associate (iv => buffers(cur)%text%intervals)
            do
                if (iv%runs(k)%length >= len) then
                    if (has_all_properties(plist, k)) exit
                    if (iv%runs(k)%length > len) then
                        call split_interval_left(iv, k, len, r)
                        k = r
                    end if
                    call add_properties(plist, k, any_change)
                    exit
                end if
                len = len - iv%runs(k)%length
                call add_properties(plist, k, any_change)
                k = k + 1
            end do
        end associate
        call signal_text_properties_change(s, e, status)
    end subroutine add_text_properties

    !> Set property `prop' to `val' on the text from `start' to `end'.
    !> Port of textprop.c:Fput_text_property.
    recursive subroutine put_text_property(start, end, prop, val, status)
        integer(int64), intent(in) :: start, end
        type(value_type), intent(in) :: prop, val
        type(status_type), intent(out) :: status
        logical :: changed
        call add_text_properties(start, end, list(prop, val), changed, status)
    end subroutine put_text_property

    !> Combine FACE with any existing faces from START to END.  FACE is
    !> combined with the existing `face' properties as a list: in front
    !> of them, or after them if APPEND is true; text without a face gets
    !> FACE itself.  Port of textprop.c:Fadd_face_text_property (in the
    !> current buffer).
    recursive subroutine add_face_text_property(start, end, face, status, append)
        integer(int64), intent(in) :: start, end
        type(value_type), intent(in) :: face
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: append
        type(value_type), allocatable :: prev_items(:), all(:)
        type(value_type) :: prev, prop
        integer(int64) :: s, e, next
        logical :: found, app
        integer :: n
        status = status_type()
        app = .false.
        if (present(append)) app = append
        prop = make_symbol_ref(intern("face"))
        s = min(start, end)
        e = max(start, end)
        do while (s < e)
            call next_single_property_change(s, prop, next, found, status, e)
            if (.not. status%ok()) return
            if (.not. found) next = e
            call get_text_property(s, prop, prev, status)
            if (.not. status%ok()) return
            if (nilp(prev)) then
                call put_text_property(s, next, prop, face, status)
            else if (consp(prev) .and. .not. keyword_list(prev)) then
                ! The previous value is a list, so prepend (or append) the
                ! new value to this list.
                call list_elements(prev, prev_items)
                n = size(prev_items)
                allocate (all(n + 1))
                if (app) then
                    all(:n) = prev_items
                    all(n + 1) = face
                else
                    all(1) = face
                    all(2:) = prev_items
                end if
                call put_text_property(s, next, prop, make_list(all), status)
                deallocate (all)
            else if (app) then
                ! The previous value is a single value, so make it into a
                ! list.
                call put_text_property(s, next, prop, list(prev, face), status)
            else
                call put_text_property(s, next, prop, list(face, prev), status)
            end if
            if (.not. status%ok()) return
            s = next
        end do
    contains

        !> Whether V is an anonymous face (a list starting with a keyword).
        logical function keyword_list(v)
            type(value_type), intent(in) :: v
            type(value_type), allocatable :: elts(:)
            character(len=:), allocatable :: name
            keyword_list = .false.
            call list_elements(v, elts)
            if (size(elts) == 0) return
            if (.not. symbolp(elts(1))) return
            name = symbols(xsymbol(elts(1)))%name
            if (len(name) > 0) keyword_list = name(1:1) == ":"
        end function keyword_list
    end subroutine add_face_text_property

    !> Replace the properties of the text from `start' to `end' with
    !> `properties'.  The change hooks run.  Port of
    !> textprop.c:Fset_text_properties (set_text_properties with
    !> COHERENT_CHANGE_P t, set_text_properties_1).
    recursive subroutine set_text_properties(start, end, properties, changed, status)
        integer(int64), intent(in) :: start, end
        type(value_type), intent(in) :: properties
        logical, intent(out) :: changed
        type(status_type), intent(out) :: status
        type(value_type) :: plist
        integer(int64) :: s, e, k, r, prev_n, prev_pos, prev_len, prev_z
        logical :: nonempty, first_time
        changed = .false.
        call validate_plist(properties, plist, status)
        if (.not. status%ok()) return
        first_time = .true.
        do
            call validate_range(start, end, s, e, nonempty, status)
            if (.not. status%ok() .or. .not. nonempty) return
            ! No properties wanted and none there: nothing to do.
            if (buffers(cur)%text%intervals%n == 0 .and. nilp(plist)) return
            call ensure_intervals()
            if (.not. first_time) exit
            associate (iv => buffers(cur)%text%intervals)
                k = find_interval(iv, s)
                prev_n = iv%n
                prev_pos = iv%runs(k)%position
                prev_len = iv%runs(k)%length
            end associate
            prev_z = buffers(cur)%text%z
            call modify_text_properties(s, e, status)
            if (.not. status%ok()) return
            ! If the intervals changed behind our back, start over.
            first_time = .false.
            if (interval_unchanged(k, prev_n, prev_pos, prev_len, prev_z)) exit
        end do
        changed = .true.
        associate (iv => buffers(cur)%text%intervals)
            k = find_interval(iv, s)
            if (iv%runs(k)%position /= s) then
                call split_interval_right(iv, k, s - iv%runs(k)%position, r)
                k = r
            end if
            do while (k <= iv%n)
                if (iv%runs(k)%position >= e) exit
                if (interval_end(iv, k) > e) then
                    call split_interval_left(iv, k, e - iv%runs(k)%position, r)
                    k = r
                end if
                iv%runs(k)%plist = plist
                k = k + 1
            end do
        end associate
        call signal_text_properties_change(s, e, status)
    end subroutine set_text_properties

    !> Remove the properties named in plist `properties' (values ignored)
    !> from the text from `start' to `end'.
    !> Port of textprop.c:Fremove_text_properties.
    recursive subroutine remove_text_properties(start, end, properties, changed, status)
        integer(int64), intent(in) :: start, end
        type(value_type), intent(in) :: properties
        logical, intent(out) :: changed
        type(status_type), intent(out) :: status
        call remove_common(start, end, properties, 2_int64, changed, status)
    end subroutine remove_text_properties

    !> Remove the properties named in `list' from the text from `start' to
    !> `end'.  Port of textprop.c:Fremove_list_of_text_properties.
    recursive subroutine remove_list_of_text_properties(start, end, list, changed, status)
        integer(int64), intent(in) :: start, end
        type(value_type), intent(in) :: list
        logical, intent(out) :: changed
        type(status_type), intent(out) :: status
        call remove_common(start, end, list, 1_int64, changed, status)
    end subroutine remove_list_of_text_properties

    recursive subroutine remove_common(start, end, names, step, changed, status)
        integer(int64), intent(in) :: start, end, step
        type(value_type), intent(in) :: names
        logical, intent(out) :: changed
        type(status_type), intent(out) :: status
        integer(int64) :: s, e, len, k, got, r, prev_n, prev_pos, prev_len, prev_z
        logical :: nonempty, any_change, first_time, done
        changed = .false.
        first_time = .true.
        do
            call validate_range(start, end, s, e, nonempty, status)
            if (.not. status%ok() .or. .not. nonempty) return
            if (buffers(cur)%text%intervals%n == 0) return
            done = .false.
            associate (iv => buffers(cur)%text%intervals)
                len = e - s
                k = find_interval(iv, s)
                ! Skip intervals that have none of the properties.
                if (.not. has_some_properties(names, step, k)) then
                    got = iv%runs(k)%length - (s - iv%runs(k)%position)
                    do
                        if (got >= len) then
                            done = .true.
                            exit
                        end if
                        len = len - got
                        k = k + 1
                        got = iv%runs(k)%length
                        if (has_some_properties(names, step, k)) exit
                    end do
                else if (iv%runs(k)%position /= s) then
                    call split_interval_right(iv, k, s - iv%runs(k)%position, r)
                    k = r
                end if
                if (.not. done) then
                    prev_n = iv%n
                    prev_pos = iv%runs(k)%position
                    prev_len = iv%runs(k)%length
                end if
            end associate
            if (done) return
            if (.not. first_time) exit
            prev_z = buffers(cur)%text%z
            call modify_text_properties(s, e, status)
            if (.not. status%ok()) return
            ! If the intervals changed behind our back, start over.
            first_time = .false.
            if (interval_unchanged(k, prev_n, prev_pos, prev_len, prev_z)) exit
        end do
        changed = .true.
        associate (iv => buffers(cur)%text%intervals)
            do
                if (iv%runs(k)%length >= len) then
                    if (.not. has_some_properties(names, step, k)) exit
                    if (iv%runs(k)%length > len) then
                        call split_interval_left(iv, k, len, r)
                        k = r
                    end if
                    call remove_properties(names, step, k, any_change)
                    exit
                end if
                len = len - iv%runs(k)%length
                call remove_properties(names, step, k, any_change)
                k = k + 1
            end do
        end associate
        call signal_text_properties_change(s, e, status)
    end subroutine remove_common

    ! ------------------------------------------------------------------
    ! Scanning for changes
    ! ------------------------------------------------------------------

    subroutine check_position(position, status)
        integer(int64), intent(in) :: position
        type(status_type), intent(out) :: status
        if (.not. (buffers(cur)%begv <= position .and. position <= buffers(cur)%zv)) then
            call range_error(status, position, position)
        end if
    end subroutine check_position

    !> Return LIMIT: `found' only if a limit was given.
    subroutine give_limit(result, found, limit)
        integer(int64), intent(out) :: result
        logical, intent(out) :: found
        integer(int64), intent(in), optional :: limit
        result = 0
        found = present(limit)
        if (found) result = limit
    end subroutine give_limit

    !> Next position after `position' where any property changes, or
    !> `limit' if none before it; `found' false means nil (no change and no
    !> limit).  Port of textprop.c:Fnext_property_change (LIMIT t pending).
    subroutine next_property_change(position, result, found, status, limit)
        integer(int64), intent(in) :: position
        integer(int64), intent(out) :: result
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: limit
        call next_change(position, result, found, status, limit=limit)
    end subroutine next_property_change

    !> Like `next_property_change', for the single property `prop'.
    !> Port of textprop.c:Fnext_single_property_change.
    subroutine next_single_property_change(position, prop, result, found, status, limit)
        integer(int64), intent(in) :: position
        type(value_type), intent(in) :: prop
        integer(int64), intent(out) :: result
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: limit
        call next_change(position, result, found, status, prop, limit)
    end subroutine next_single_property_change

    subroutine next_change(position, result, found, status, prop, limit)
        integer(int64), intent(in) :: position
        integer(int64), intent(out) :: result
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        type(value_type), intent(in), optional :: prop
        integer(int64), intent(in), optional :: limit
        integer(int64) :: k, next, bound
        type(value_type) :: here
        call give_limit(result, found, limit)
        call check_position(position, status)
        if (.not. status%ok()) return
        k = interval_at(position)
        if (k == 0) return
        associate (iv => buffers(cur)%text%intervals)
            if (present(prop)) here = textget(iv%runs(k)%plist, prop)
            next = k + 1
            do while (next <= iv%n)
                if (present(limit)) then
                    if (iv%runs(next)%position >= limit) exit
                end if
                if (present(prop)) then
                    if (.not. value_eq(here, textget(iv%runs(next)%plist, prop))) exit
                else
                    if (.not. intervals_equal(iv%runs(k)%plist, iv%runs(next)%plist)) exit
                end if
                next = next + 1
            end do
            bound = buffers(cur)%zv
            if (present(limit)) bound = limit
            if (next > iv%n) return
            if (iv%runs(next)%position >= bound) return
            result = iv%runs(next)%position
            found = .true.
        end associate
    end subroutine next_change

    !> Previous position before `position' where any property changes, or
    !> `limit'.  Port of textprop.c:Fprevious_property_change.
    subroutine previous_property_change(position, result, found, status, limit)
        integer(int64), intent(in) :: position
        integer(int64), intent(out) :: result
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: limit
        call previous_change(position, result, found, status, limit=limit)
    end subroutine previous_property_change

    !> Like `previous_property_change', for the single property `prop'.
    !> Port of textprop.c:Fprevious_single_property_change.
    subroutine previous_single_property_change(position, prop, result, found, status, limit)
        integer(int64), intent(in) :: position
        type(value_type), intent(in) :: prop
        integer(int64), intent(out) :: result
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: limit
        call previous_change(position, result, found, status, prop, limit)
    end subroutine previous_single_property_change

    subroutine previous_change(position, result, found, status, prop, limit)
        integer(int64), intent(in) :: position
        integer(int64), intent(out) :: result
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        type(value_type), intent(in), optional :: prop
        integer(int64), intent(in), optional :: limit
        integer(int64) :: k, prev, bound
        type(value_type) :: here
        call give_limit(result, found, limit)
        call check_position(position, status)
        if (.not. status%ok()) return
        associate (b => buffers(cur), iv => buffers(cur)%text%intervals)
            if (iv%n == 0 .or. b%begv == b%zv) return
            ! Start with the interval containing the char before POSITION.
            k = find_interval(iv, position)
            if (iv%runs(k)%position == position) k = k - 1
            if (k < 1) return
            if (present(prop)) here = textget(iv%runs(k)%plist, prop)
            prev = k - 1
            do while (prev >= 1)
                if (present(limit)) then
                    if (interval_end(iv, prev) <= limit) exit
                end if
                if (present(prop)) then
                    if (.not. value_eq(here, textget(iv%runs(prev)%plist, prop))) exit
                else
                    if (.not. intervals_equal(iv%runs(prev)%plist, iv%runs(k)%plist)) exit
                end if
                prev = prev - 1
            end do
            bound = b%begv
            if (present(limit)) bound = limit
            if (prev < 1) return
            if (interval_end(iv, prev) <= bound) return
            result = interval_end(iv, prev)
            found = .true.
        end associate
    end subroutine previous_change

    !> First position in `start'..`end' whose `prop' is eq to `value';
    !> `found' false means nil.  Port of textprop.c:Ftext_property_any.
    subroutine text_property_any(start, end, prop, value, result, found, status)
        integer(int64), intent(in) :: start, end
        type(value_type), intent(in) :: prop, value
        integer(int64), intent(out) :: result
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        call property_scan(start, end, prop, value, .true., result, found, status)
    end subroutine text_property_any

    !> First position in `start'..`end' whose `prop' is not eq to `value'.
    !> Port of textprop.c:Ftext_property_not_all.
    subroutine text_property_not_all(start, end, prop, value, result, found, status)
        integer(int64), intent(in) :: start, end
        type(value_type), intent(in) :: prop, value
        integer(int64), intent(out) :: result
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        call property_scan(start, end, prop, value, .false., result, found, status)
    end subroutine text_property_not_all

    subroutine property_scan(start, end, prop, value, want_eq, result, found, status)
        integer(int64), intent(in) :: start, end
        type(value_type), intent(in) :: prop, value
        logical, intent(in) :: want_eq
        integer(int64), intent(out) :: result
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: s, e, k
        logical :: nonempty
        result = 0
        found = .false.
        call validate_range(start, end, s, e, nonempty, status)
        if (.not. status%ok()) return
        if (.not. nonempty .or. buffers(cur)%text%intervals%n == 0) then
            ! No intervals: every character has PROP nil.
            if (nonempty .and. (nilp(value) .eqv. want_eq)) then
                result = s
                found = .true.
            end if
            return
        end if
        associate (iv => buffers(cur)%text%intervals)
            k = find_interval(iv, s)
            do while (k <= iv%n)
                if (iv%runs(k)%position >= e) exit
                if (value_eq(textget(iv%runs(k)%plist, prop), value) .eqv. want_eq) then
                    result = max(iv%runs(k)%position, s)
                    found = .true.
                    return
                end if
                k = k + 1
            end do
        end associate
    end subroutine property_scan

    !> Whether `prop' at `pos' would be inherited by text inserted there:
    !> -1 from the character before (rear-sticky), 1 from the one after
    !> (front-sticky), 0 from neither.  Port of
    !> textprop.c:text_property_stickiness (current buffer).
    integer function text_property_stickiness(prop, pos) result(stickiness)
        type(value_type), intent(in) :: prop
        integer(int64), intent(in) :: pos
        type(value_type) :: rear_non_sticky, front_sticky, prev_value
        type(status_type) :: status
        logical :: ignore_previous_character, is_rear_sticky, is_front_sticky
        is_rear_sticky = .true.
        is_front_sticky = .false.
        ignore_previous_character = pos <= buffers(cur)%begv
        ! text-property-default-nonsticky: its standard value makes
        ! syntax-table and display (textprop.c), composition (composite.c)
        ! and fill-space (fill.el) rear-nonsticky.
        if (ignore_previous_character .or. default_nonsticky(prop)) then
            is_rear_sticky = .false.
        else
            call get_text_property(pos - 1, qsym("rear_nonsticky"), rear_non_sticky, status)
            if (consp(rear_non_sticky)) then
                if (memq_p(prop, rear_non_sticky)) is_rear_sticky = .false.
            else if (.not. nilp(rear_non_sticky)) then
                is_rear_sticky = .false.
            end if
        end if
        call get_text_property(pos, qsym("front_sticky"), front_sticky, status)
        if (value_eq(front_sticky, qsym("t"))) then
            is_front_sticky = .true.
        else if (consp(front_sticky)) then
            if (memq_p(prop, front_sticky)) is_front_sticky = .true.
        end if
        if (is_rear_sticky .and. .not. is_front_sticky) then
            stickiness = -1
        else if (.not. is_rear_sticky .and. is_front_sticky) then
            stickiness = 1
        else if (.not. is_rear_sticky .and. .not. is_front_sticky) then
            stickiness = 0
        else
            ! Inconsistent: rear-sticky wins unless its value is nil.
            stickiness = -1
            if (ignore_previous_character) then
                stickiness = 1
            else
                call get_text_property(pos - 1, prop, prev_value, status)
                if (nilp(prev_value)) stickiness = 1
            end if
        end if
    contains

        logical function default_nonsticky(p)
            type(value_type), intent(in) :: p
            default_nonsticky = value_eq(p, qsym("fill_space")) .or. value_eq(p, qsym("composition")) .or. &
                                value_eq(p, qsym("syntax_table")) .or. value_eq(p, qsym("display"))
        end function default_nonsticky
    end function text_property_stickiness

    !> How PROPVAL, a value of the `invisible' property, makes text
    !> invisible in the current buffer: 0 not at all, 1 hidden, 2 hidden
    !> and replaced by an ellipsis (`buffer_invisibility_spec' t: any
    !> non-nil value hides text).  Port of xdisp.c:invisible_prop with
    !> TEXT_PROP_MEANS_INVISIBLE.
    integer function text_prop_means_invisible(propval) result(invis)
        type(value_type), intent(in) :: propval
        type(value_type), allocatable :: elts(:)
        integer :: k
        invis = 0
        if (cur == 0) return
        if (buffers(cur)%invisibility_spec_t) then
            if (.not. nilp(propval)) invis = 1
            return
        end if
        if (.not. allocated(buffers(cur)%invisibility_spec)) return
        invis = match(propval)
        if (invis /= 0) return
        if (consp(propval)) then
            call list_elements(propval, elts)
            do k = 1, size(elts)
                invis = match(elts(k))
                if (invis /= 0) return
            end do
        end if
    contains

        integer function match(v) result(m)
            type(value_type), intent(in) :: v
            integer :: j
            m = 0
            associate (spec => buffers(cur)%invisibility_spec)
                do j = 1, size(spec)
                    if (.not. value_eq(v, spec(j)%atom)) cycle
                    m = 1
                    if (spec(j)%cons .and. spec(j)%ellipsis) m = 2
                    return
                end do
            end associate
        end function match
    end function text_prop_means_invisible

    !> Return the value of POSITION's property PROP, in the current buffer.
    !> Almost identical to `get_char_property' except for the following
    !> difference: Whereas `get_char_property' returns the property of the
    !> char at (i.e. right after) POSITION, this pays attention to
    !> properties's stickiness and overlays's advancement settings, in
    !> order to find the property of POSITION itself, i.e. the property
    !> that a char would inherit if it were inserted at POSITION.  Port of
    !> editfns.c:Fget_pos_property (no `window' properties).
    function get_pos_property(position, prop) result(val)
        integer(int64), intent(in) :: position
        type(value_type), intent(in) :: prop
        type(value_type) :: val
        type(handle_type), allocatable :: vec(:)
        type(handle_type) :: best
        type(value_type) :: tem
        type(status_type) :: status
        integer(int64) :: b, e, p1, s1, p2, s2
        logical :: fa, ra
        integer :: k
        val = nil()
        if (cur == 0) return
        ! First check if there's an overlay at POS.
        best = null_handle()
        call overlays_in(max(position - 1, 1_int64), position + 1, vec)
        do k = 1, size(vec)
            tem = overlay_get_prop(vec(k), prop)
            if (nilp(tem)) cycle
            b = overlay_start(vec(k))
            e = overlay_end(vec(k))
            call overlay_advance(vec(k), fa, ra)
            ! Check the overlay is indeed active at point.
            if ((b == position .and. fa) .or. (e == position .and. .not. ra) .or. b > position .or. e < position) cycle
            if (best%index /= 0) then
                call overlay_priority(best, p1, s1)
                call overlay_priority(vec(k), p2, s2)
                if (compare_overlays(p1, s1, overlay_start(best), overlay_end(best), int(best%index, int64), &
                                     p2, s2, b, e, int(vec(k)%index, int64)) >= 0) cycle
            end if
            best = vec(k)
            val = tem
        end do
        if (best%index /= 0) return
        ! Now check the text properties.
        select case (text_property_stickiness(prop, position))
        case (1:)
            call get_text_property(position, prop, val, status)
        case (:-1)
            if (position > buffers(cur)%begv) call get_text_property(position - 1, prop, val, status)
        case default
            val = nil()
        end select
        if (.not. status%ok()) val = nil()
    end function get_pos_property

    !> The value VAL of text property PROP at POS and the range [BEG, END)
    !> of the text around POS where it has that value (`eq').  Port of
    !> intervals.c:get_property_and_range for the current buffer.
    subroutine get_property_and_range(pos, prop, val, beg, end)
        integer(int64), intent(in) :: pos
        type(value_type), intent(in) :: prop
        type(value_type), intent(out) :: val
        integer(int64), intent(out) :: beg, end
        type(status_type) :: status
        logical :: found
        call get_text_property(pos, prop, val, status)
        call previous_single_property_change(pos + 1, prop, beg, found, status, buffers(cur)%begv)
        if (.not. found) beg = buffers(cur)%begv
        call next_single_property_change(pos, prop, end, found, status, buffers(cur)%zv)
        if (.not. found) end = buffers(cur)%zv
    end subroutine get_property_and_range

    !> Return the value of POSITION's property PROP, in the current
    !> buffer.  Both overlay properties and text properties are checked:
    !> of the overlays containing POSITION with a PROP property, the one
    !> with the highest priority gives the value, else the text property
    !> does.  Port of textprop.c:Fget_char_property (OBJECT the current
    !> buffer; no `window' properties).
    function get_char_property(position, prop) result(val)
        integer(int64), intent(in) :: position
        type(value_type), intent(in) :: prop
        type(value_type) :: val
        type(handle_type) :: overlay
        call get_char_property_and_overlay(position, prop, val, overlay)
    end function get_char_property

    !> Like `get_char_property', but with extra info on where the value
    !> came from: OVERLAY is the overlay it came from, the null handle if
    !> it is a text property (or nil).  Port of
    !> textprop.c:get_char_property_and_overlay for the current buffer.
    subroutine get_char_property_and_overlay(position, prop, val, overlay)
        integer(int64), intent(in) :: position
        type(value_type), intent(in) :: prop
        type(value_type), intent(out) :: val
        type(handle_type), intent(out) :: overlay
        type(handle_type), allocatable :: vec(:)
        type(status_type) :: status
        integer :: k
        overlay = null_handle()
        val = nil()
        if (cur == 0) return
        ! The overlays containing POSITION, highest priority first.
        call overlays_at(position, vec, sorted=.true.)
        do k = 1, size(vec)
            val = overlay_get_prop(vec(k), prop)
            if (.not. nilp(val)) then
                overlay = vec(k)
                return
            end if
        end do
        call get_text_property(position, prop, val, status)
        if (.not. status%ok()) val = nil()
    end subroutine get_char_property_and_overlay

    !> Return the position of next text property or overlay change.
    !> This scans characters forward in the current buffer from POSITION
    !> till it finds a change in some text property, or the beginning or
    !> end of an overlay, and returns the position of that.  If none is
    !> found, and LIMIT is absent, the function returns `point_max'.
    !> If the optional second argument LIMIT is present, don't search past
    !> position LIMIT; return LIMIT if nothing is found before LIMIT.
    !> LIMIT is a no-op if it is greater than `point_max'.  Port of
    !> textprop.c:Fnext_char_property_change.
    integer(int64) function next_char_property_change(position, limit) result(next)
        integer(int64), intent(in) :: position
        integer(int64), intent(in), optional :: limit
        integer(int64) :: temp
        type(status_type) :: status
        logical :: found
        temp = next_overlay_change(position)
        if (present(limit)) temp = min(temp, limit)
        call next_property_change(position, next, found, status, temp)
        if (.not. found .or. .not. status%ok()) next = temp
    end function next_char_property_change

    !> Return the position of previous text property or overlay change.
    !> Scans characters backward in the current buffer from POSITION till
    !> it finds a change in some text property, or the beginning or end of
    !> an overlay, and returns the position of that.  If none is found, and
    !> LIMIT is absent, the function returns `point_min'.  If the optional
    !> second argument LIMIT is present, don't search back past position
    !> LIMIT; return LIMIT if nothing is found before LIMIT.  LIMIT is a
    !> no-op if it is less than `point_min'.  Port of
    !> textprop.c:Fprevious_char_property_change.
    integer(int64) function previous_char_property_change(position, limit) result(prev)
        integer(int64), intent(in) :: position
        integer(int64), intent(in), optional :: limit
        integer(int64) :: temp
        type(status_type) :: status
        logical :: found
        temp = previous_overlay_change(position)
        if (present(limit)) temp = max(temp, limit)
        call previous_property_change(position, prev, found, status, temp)
        if (.not. found .or. .not. status%ok()) prev = temp
    end function previous_char_property_change

    !> Return the position of next text property or overlay change for a
    !> specific property.  Scans characters forward from POSITION till it
    !> finds a change in the PROP property, then returns the position of
    !> the change.  The property values are compared with `eq'.  If the
    !> property is constant all the way to the end of the buffer, return
    !> the last valid position in the buffer, or LIMIT if present and not
    !> past it.  Port of textprop.c:Fnext_single_char_property_change for
    !> the current buffer.
    integer(int64) function next_single_char_property_change(position, prop, limit) result(next)
        integer(int64), intent(in) :: position
        type(value_type), intent(in) :: prop
        integer(int64), intent(in), optional :: limit
        type(value_type) :: initial_value, value
        integer(int64) :: lim
        lim = buffers(cur)%zv
        if (present(limit)) lim = limit
        if (position >= lim) then
            next = lim
            if (next > buffers(cur)%zv) next = buffers(cur)%zv
            return
        end if
        initial_value = get_char_property(position, prop)
        next = position
        do
            next = next_char_property_change(next, lim)
            if (next >= lim) then
                next = lim
                exit
            end if
            value = get_char_property(next, prop)
            if (.not. value_eq(value, initial_value)) exit
            if (next >= buffers(cur)%zv) exit
        end do
    end function next_single_char_property_change

    !> Return the position of previous text property or overlay change
    !> for a specific property.  Scans characters backward from POSITION
    !> till it finds a change in the PROP property, then returns the
    !> position of the change.  The property values are compared with
    !> `eq'.  If the property is constant all the way to the start of the
    !> buffer, return the first valid position in the buffer, or LIMIT if
    !> present and not before it.  Port of
    !> textprop.c:Fprevious_single_char_property_change for the current
    !> buffer.
    integer(int64) function previous_single_char_property_change(position, prop, limit) result(prev)
        integer(int64), intent(in) :: position
        type(value_type), intent(in) :: prop
        integer(int64), intent(in), optional :: limit
        type(value_type) :: initial_value, value
        integer(int64) :: lim
        lim = buffers(cur)%begv
        if (present(limit)) lim = limit
        if (position <= lim) then
            prev = lim
            if (prev < buffers(cur)%begv) prev = buffers(cur)%begv
            return
        end if
        initial_value = get_char_property(position - 1, prop)
        prev = position
        do
            prev = previous_char_property_change(prev, lim)
            if (prev <= lim) then
                prev = lim
                exit
            end if
            value = get_char_property(prev - 1, prop)
            if (.not. value_eq(value, initial_value)) exit
        end do
    end function previous_single_char_property_change

    ! ------------------------------------------------------------------
    ! Read-only text
    ! ------------------------------------------------------------------

    !> Signal text-read-only (or buffer-read-only) if `start'..`end' may not
    !> be changed because of `read_only' / `inhibit_read_only' properties.
    !> An empty range is an insertion: then the stickiness of `read_only'
    !> on the neighbours decides.  Port of textprop.c:
    !> verify_interval_modification (modification hooks pending).
    subroutine verify_interval_modification(start_in, end_in, status)
        integer(int64), intent(in) :: start_in, end_in
        type(status_type), intent(out) :: status
        type(value_type) :: inhibit, read_only, after, before, tem, val
        integer(int64) :: start, end, k, prev, p
        logical :: buffer_read_only, implied, express, has_after, has_prev, found
        associate (b => buffers(cur), iv => buffers(cur)%text%intervals)
            if (iv%n == 0) return
            start = min(start_in, end_in)
            end = max(start_in, end_in)
            inhibit = inhibit_read_only()
            if (start == end) then
                ! I: interval after START; PREV: interval before it.
                k = find_interval(iv, start)
                has_prev = .false.
                prev = 0
                if (start /= b%begv) then
                    if (iv%runs(k)%position == start) then
                        prev = k - 1
                    else
                        prev = k
                    end if
                    has_prev = prev >= 1
                end if
                has_after = start /= b%zv
                if (.not. (nilp(inhibit) .or. consp(inhibit))) return
                if (.not. (has_after .and. has_prev .and. k == prev)) then
                    if (has_after) then
                        after = textget(iv%runs(k)%plist, qsym("read_only"))
                        if (.not. nilp(after) .and. .not. memq_p(after, inhibit)) then
                            tem = textget(iv%runs(k)%plist, qsym("front_sticky"))
                            call plist_get_eq(iv%runs(k)%plist, qsym("read_only"), val, found)
                            if (tmem(qsym("read_only"), tem) .or. &
                                (nilp(val) .and. tmem(qsym("category"), tem))) then
                                call text_read_only(status, after)
                                return
                            end if
                        end if
                    end if
                    if (has_prev) then
                        before = textget(iv%runs(prev)%plist, qsym("read_only"))
                        if (.not. nilp(before) .and. .not. memq_p(before, inhibit)) then
                            tem = textget(iv%runs(prev)%plist, qsym("rear_nonsticky"))
                            call plist_get_eq(iv%runs(prev)%plist, qsym("read_only"), val, found)
                            if (.not. tmem(qsym("read_only"), tem) .and. &
                                (.not. nilp(val) .or. .not. tmem(qsym("category"), tem))) then
                                call text_read_only(status, before)
                                return
                            end if
                        end if
                    end if
                else
                    ! Inside one interval: it is read-only unless both ends say
                    ! the property does not stick.
                    after = textget(iv%runs(k)%plist, qsym("read_only"))
                    if (.not. nilp(after) .and. .not. memq_p(after, inhibit)) then
                        call plist_get_eq(iv%runs(k)%plist, qsym("read_only"), val, found)
                        tem = textget(iv%runs(k)%plist, qsym("front_sticky"))
                        if (tmem(qsym("read_only"), tem) .or. &
                            (nilp(val) .and. tmem(qsym("category"), tem))) then
                            call text_read_only(status, after)
                            return
                        end if
                        tem = textget(iv%runs(k)%plist, qsym("rear_nonsticky"))
                        if (.not. tmem(qsym("read_only"), tem) .and. &
                            (.not. nilp(val) .or. .not. tmem(qsym("category"), tem))) then
                            call text_read_only(status, after)
                            return
                        end if
                    end if
                end if
            else
                buffer_read_only = b%read_only .and. nilp(inhibit)
                k = find_interval(iv, start)
                p = 0
                do
                    read_only = textget(iv%runs(k)%plist, qsym("read_only"))
                    ! INTERVAL_GENERALLY_WRITABLE_P / INTERVAL_EXPRESSLY_WRITABLE_P
                    implied = nilp(read_only) .or. (.not. nilp(inhibit) .and. .not. consp(inhibit))
                    express = .not. nilp(textget(iv%runs(k)%plist, qsym("inhibit_read_only")))
                    if (.not. express .and. .not. nilp(read_only) .and. consp(inhibit)) then
                        express = memq_p(read_only, inhibit)
                    end if
                    if (.not. implied .and. .not. express) then
                        call text_read_only(status, read_only)
                        return
                    else if (.not. express .and. buffer_read_only) then
                        call set_error(status, FM_BUFFER_READ_ONLY, b%name)
                        return
                    end if
                    p = interval_end(iv, k)
                    k = k + 1
                    if (k > iv%n) exit
                    if (iv%runs(k)%position >= end) exit
                end do
                ! The last writable interval must reach END if the buffer is
                ! read-only as a whole.
                if (p < end .and. buffer_read_only) then
                    call set_error(status, FM_BUFFER_READ_ONLY, b%name)
                    return
                end if
            end if
        end associate
    end subroutine verify_interval_modification
    ! ------------------------------------------------------------------
    ! Properties of strings
    ! ------------------------------------------------------------------

    !> Property list of the character at POSITION (0-based) of STRING; nil
    !> at its end or where it has none.  Port of textprop.c:Ftext_properties_at
    !> with a string OBJECT.
    function string_text_properties_at(position, string) result(plist)
        integer(int64), intent(in) :: position
        type(value_type), intent(in) :: string
        type(value_type) :: plist
        integer(int64), allocatable :: starts(:), ends(:)
        type(value_type), allocatable :: plists(:)
        integer :: k
        call string_intervals(string, starts, ends, plists)
        do k = 1, size(starts)
            if (starts(k) <= position .and. position < ends(k)) then
                plist = plists(k)
                return
            end if
        end do
    end function string_text_properties_at

    !> Value of PROP on the character at POSITION (0-based) of STRING (via
    !> `category' too).  Port of textprop.c:Fget_text_property with a
    !> string OBJECT.
    function string_get_text_property(position, prop, string) result(val)
        integer(int64), intent(in) :: position
        type(value_type), intent(in) :: prop, string
        type(value_type) :: val
        val = textget(string_text_properties_at(position, string), prop)
    end function string_get_text_property

    !> Add the properties of PROPERTIES (a plist) to the characters START
    !> to END (0-based, either order) of STRING; CHANGED says whether any
    !> property changed.  Port of textprop.c:Fadd_text_properties with a
    !> string OBJECT.
    subroutine string_add_text_properties(start, end, properties, string, changed, status)
        integer(int64), intent(in) :: start, end
        type(value_type), intent(in) :: properties
        type(value_type), intent(inout) :: string
        logical, intent(out) :: changed
        type(status_type), intent(out) :: status
        integer(int64), allocatable :: starts(:), ends(:), cuts(:)
        type(value_type), allocatable :: plists(:), pieces(:)
        type(value_type) :: plist
        integer(int64) :: s, e, nchars
        integer :: k, j
        logical :: piece_changed
        changed = .false.
        call string_chars(string, nchars, status)
        if (.not. status%ok()) return
        s = min(start, end)
        e = max(start, end)
        if (s < 0 .or. e > nchars) then
            call range_error(status, start, end)
            return
        end if
        call validate_plist(properties, plist, status)
        if (.not. status%ok()) return
        if (s == e .or. nilp(plist)) return
        call string_intervals(string, starts, ends, plists)
        ! Cut the string where an interval or the range starts or ends,
        ! and give each piece in the range the properties.
        cuts = [0_int64, nchars, s, e, starts, ends]
        call sort_unique(cuts)
        allocate (pieces(size(cuts) - 1))
        do j = 1, size(pieces)
            do k = 1, size(starts)
                if (starts(k) <= cuts(j) .and. cuts(j) < ends(k)) pieces(j) = plists(k)
            end do
            if (s <= cuts(j) .and. cuts(j) < e) then
                call plist_add(pieces(j), plist, piece_changed)
                changed = changed .or. piece_changed
            end if
        end do
        if (changed) call set_merged_intervals(string, cuts, pieces)
    end subroutine string_add_text_properties

    !> Set one property of the characters START to END (0-based) of
    !> STRING.  Port of textprop.c:Fput_text_property with a string OBJECT.
    subroutine string_put_text_property(start, end, prop, val, string, status)
        integer(int64), intent(in) :: start, end
        type(value_type), intent(in) :: prop, val
        type(value_type), intent(inout) :: string
        type(status_type), intent(out) :: status
        logical :: changed
        call string_add_text_properties(start, end, list(prop, val), string, changed, status)
    end subroutine string_put_text_property

    !> A copy of STRING with the properties of PROPERTIES (a plist) on all
    !> of it, added to those it has.  Port of editfns.c:Fpropertize.
    function propertize(string, properties) result(v)
        type(value_type), intent(in) :: string, properties
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: nchars
        logical :: changed
        v = string
        call string_chars(v, nchars, st)
        if (st%ok()) call string_add_text_properties(0_int64, nchars, properties, v, changed, st)
    end function propertize

    !> The characters of STRING (a wrong-type-argument if not a string).
    subroutine string_chars(string, nchars, status)
        type(value_type), intent(in) :: string
        integer(int64), intent(out) :: nchars
        type(status_type), intent(out) :: status
        type(status_type) :: st
        nchars = 0
        if (.not. stringp(string)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "stringp")
            return
        end if
        call multibyte_chars_in_text(xstring(string), nchars, st)
        if (.not. st%ok()) nchars = len(xstring(string), kind=int64)
    end subroutine string_chars

    !> Add the pairs of NEW to PLIST, replacing values of properties it
    !> has (textprop.c:add_properties on a plist).
    subroutine plist_add(plist, new, changed)
        type(value_type), intent(inout) :: plist
        type(value_type), intent(in) :: new
        logical, intent(out) :: changed
        type(value_type), allocatable :: pairs(:), old(:), grown(:)
        integer(int64) :: j, m
        logical :: found
        changed = .false.
        call list_elements(new, pairs)
        do j = 1, size(pairs, kind=int64) - 1, 2
            call list_elements(plist, old)
            found = .false.
            do m = 1, size(old, kind=int64) - 1, 2
                if (value_eq(old(m), pairs(j))) then
                    found = .true.
                    if (.not. value_eq(old(m + 1), pairs(j + 1))) then
                        old(m + 1) = pairs(j + 1)
                        plist = make_list(old)
                        changed = .true.
                    end if
                    exit
                end if
            end do
            if (.not. found) then
                grown = [pairs(j:j + 1), old]
                plist = make_list(grown)
                changed = .true.
            end if
        end do
    end subroutine plist_add

    !> Sort CUTS and drop repeated values.
    pure subroutine sort_unique(cuts)
        integer(int64), allocatable, intent(inout) :: cuts(:)
        integer(int64) :: t
        integer :: i, j, n
        do i = 2, size(cuts)
            t = cuts(i)
            j = i - 1
            do while (j >= 1)
                if (cuts(j) <= t) exit
                cuts(j + 1) = cuts(j)
                j = j - 1
            end do
            cuts(j + 1) = t
        end do
        n = min(1, size(cuts))
        do i = 2, size(cuts)
            if (cuts(i) /= cuts(n)) then
                n = n + 1
                cuts(n) = cuts(i)
            end if
        end do
        cuts = cuts(1:n)
    end subroutine sort_unique

    !> Give STRING the pieces [CUTS(j), CUTS(j+1)) with plists PIECES(j),
    !> adjacent pieces with equal plists as one interval.
    subroutine set_merged_intervals(string, cuts, pieces)
        type(value_type), intent(inout) :: string
        integer(int64), intent(in) :: cuts(:)
        type(value_type), intent(in) :: pieces(:)
        integer(int64), allocatable :: starts(:), ends(:)
        type(value_type), allocatable :: plists(:)
        integer :: j, n
        allocate (starts(size(pieces)), ends(size(pieces)), plists(size(pieces)))
        n = 0
        do j = 1, size(pieces)
            if (n > 0) then
                if (ends(n) == cuts(j) .and. intervals_equal(plists(n), pieces(j))) then
                    ends(n) = cuts(j + 1)
                    cycle
                end if
            end if
            n = n + 1
            starts(n) = cuts(j)
            ends(n) = cuts(j + 1)
            plists(n) = pieces(j)
        end do
        call set_string_intervals(string, starts(1:n), ends(1:n), plists(1:n))
    end subroutine set_merged_intervals

    !> The position (0-based) after POSITION of STRING where property PROP
    !> changes; FOUND is false if it does not change before the end.
    !> Port of textprop.c:Fnext_single_property_change with a string
    !> OBJECT (no LIMIT).
    subroutine string_next_single_property_change(position, prop, string, result, found)
        integer(int64), intent(in) :: position
        type(value_type), intent(in) :: prop, string
        integer(int64), intent(out) :: result
        logical, intent(out) :: found
        integer(int64), allocatable :: starts(:), ends(:)
        type(value_type), allocatable :: plists(:)
        type(value_type) :: here
        type(status_type) :: st
        integer(int64) :: nchars, p
        integer :: k
        result = 0
        found = .false.
        call string_chars(string, nchars, st)
        if (.not. st%ok()) return
        call string_intervals(string, starts, ends, plists)
        here = string_get_text_property(position, prop, string)
        ! The next interval boundary where PROP's value differs.
        p = position
        do
            result = nchars
            do k = 1, size(starts)
                if (starts(k) > p) result = min(result, starts(k))
                if (ends(k) > p) result = min(result, ends(k))
            end do
            if (result >= nchars) return
            if (.not. value_eq(string_get_text_property(result, prop, string), here)) exit
            p = result
        end do
        found = .true.
    end subroutine string_next_single_property_change

    !> The characters FROM to TO (0-based, FROM <= TO, within it) of
    !> STRING with their text properties.  Port of fns.c:Fsubstring (the
    !> range already checked).
    function propertized_substring(string, from, to) result(v)
        type(value_type), intent(in) :: string
        integer(int64), intent(in) :: from, to
        type(value_type) :: v
        integer(int64), allocatable :: starts(:), ends(:)
        type(value_type), allocatable :: plists(:)
        character(len=:), allocatable :: text
        integer(int64) :: first, last
        text = xstring(string)
        first = char_byte(text, from)
        last = char_byte(text, to)
        v = make_string(text(first:last - 1))
        call string_intervals(string, starts, ends, plists)
        starts = min(max(starts, from), to) - from
        ends = min(max(ends, from), to) - from
        call set_string_intervals(v, starts, ends, plists)
    end function propertized_substring

    !> STRING1 followed by STRING2, with the text properties of both.
    !> Port of fns.c:concat_to_string for two strings.
    function propertized_concat(string1, string2) result(v)
        type(value_type), intent(in) :: string1, string2
        type(value_type) :: v
        integer(int64), allocatable :: s1(:), e1(:), s2(:), e2(:), starts(:), ends(:)
        type(value_type), allocatable :: p1(:), p2(:), plists(:)
        type(status_type) :: st
        integer(int64) :: n1
        integer :: k
        v = make_string(xstring(string1)//xstring(string2))
        call string_chars(string1, n1, st)
        call string_intervals(string1, s1, e1, p1)
        call string_intervals(string2, s2, e2, p2)
        if (size(p1) + size(p2) == 0) return
        ! (No array constructors: a zero-size one reallocates to size 0.)
        allocate (plists(size(p1) + size(p2)), starts(size(p1) + size(p2)), ends(size(p1) + size(p2)))
        do k = 1, size(p1)
            plists(k) = p1(k)
            starts(k) = s1(k)
            ends(k) = e1(k)
        end do
        do k = 1, size(p2)
            plists(size(p1) + k) = p2(k)
            starts(size(p1) + k) = s2(k) + n1
            ends(size(p1) + k) = e2(k) + n1
        end do
        call set_string_intervals(v, starts, ends, plists)
    end function propertized_concat

    !> The byte (1-based) where character N (0-based) of TEXT starts; one
    !> past the end for N at or past its end.
    pure integer(int64) function char_byte(text, n) result(byte)
        character(len=*), intent(in) :: text
        integer(int64), intent(in) :: n
        integer(int64) :: count
        integer :: i
        byte = len(text) + 1
        count = 0
        do i = 1, len(text)
            if (iand(iachar(text(i:i)), 192) /= 128) then
                if (count == n) then
                    byte = i
                    return
                end if
                count = count + 1
            end if
        end do
    end function char_byte

end module textprop_fm
