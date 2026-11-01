! bidi_fm.f90 -- the Unicode Bidirectional Algorithm (UBA).
! Ported from GNU Emacs 31.1 src/bidi.c (and the bidi parts of
! src/dispextern.h).
!
! Copyright (C) 2000-2001, 2004-2005, 2009-2026 Free Software Foundation,
! Inc.
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

!> The bidi iterator of bidi.c: it walks the text of the current buffer or
!> of a string in logical order, resolving the embedding level of each
!> character as UAX#9 says (explicit embeddings, overrides and isolates,
!> weak and neutral types, paired brackets), and in visual order with
!> `bidi_move_to_visually_next', keeping the states it looked ahead at in
!> a cache as upstream does.  Character positions in a string are
!> 0-based, as upstream; in a buffer 1-based.
!>
!> Deferred: display properties during the iteration
!> (compute_display_string_pos and compute_display_string_end of xdisp.c:
!> text a display string replaces counts as its characters), the
!> paragraph region cache (`cache_long_scans'), the buffer-local
!> `bidi_paragraph_start_regexp' and `bidi_paragraph_separate_regexp' (their
!> defaults are used), the redisplay ticks (`max_redisplay_ticks').
module bidi_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type
    use character_fm, only: MAX_CHAR, string_char_and_length, bytes_by_char_head
    use buffer_fm, only: buffers, current, buf_fetch_char, buf_fetch_byte, buf_next_char_len, buf_prev_char_len
    use marker_fm, only: char_to_byte, byte_to_char
    use search_fm, only: find_newline_no_quit, fast_looking_at
    use uni_bidi_fm, only: bidi_runs, bidi_starts, bidi_ends, bidi_codes, mirror_count, mirror_from, mirror_to, &
                           open_bracket_count, open_brackets, close_bracket_count, close_brackets
    implicit none(type, external)
    private

    public :: bidi_get_type, bidi_mirror_char, bidi_init_it, bidi_paragraph_init, bidi_move_to_visually_next, &
              bidi_find_first_overridden, bidi_level_start, bidi_push_it, bidi_pop_it, bidi_shelve_cache, &
              bidi_unshelve_cache, bidi_set_string, bidi_at_base_level

    !> The deepest embedding level (dispextern.h BIDI_MAXDEPTH).
    integer, parameter, public :: BIDI_MAXDEPTH = 125

    !> The bidirectional character types (dispextern.h bidi_type_t).
    integer, parameter, public :: UNKNOWN_BT = 0, STRONG_L = 1, STRONG_R = 2, WEAK_EN = 3, WEAK_AN = 4, &
                                  WEAK_BN = 5, NEUTRAL_B = 6, STRONG_AL = 7, LRE = 8, LRO = 9, RLE = 10, &
                                  RLO = 11, PDF = 12, LRI = 13, RLI = 14, FSI = 15, PDI = 16, WEAK_ES = 17, &
                                  WEAK_ET = 18, WEAK_CS = 19, WEAK_NSM = 20, NEUTRAL_S = 21, NEUTRAL_WS = 22, &
                                  NEUTRAL_ON = 23

    !> The paired bracket types (dispextern.h bidi_bracket_type_t).
    integer, parameter, public :: BIDI_BRACKET_NONE = 1, BIDI_BRACKET_OPEN = 2, BIDI_BRACKET_CLOSE = 3

    !> The basic directions (dispextern.h bidi_dir_t).
    integer, parameter, public :: NEUTRAL_DIR = 0, L2R = 1, R2L = 2

    !> The end of the text, as a character (bidi.c BIDI_EOB).
    integer(int32), parameter, public :: BIDI_EOB = -1

    !> The categories of bidi types (bidi.c bidi_category_t).
    integer, parameter :: UNKNOWN_BC = 0, NEUTRAL = 1, WEAK = 2, STRONG = 3, EXPLICIT_FORMATTING = 4

    !> Inhibit the bidi bracket pair algorithm (BPA).  If true, the paired
    !> brackets are resolved as other neutral characters.  Port of xdisp.c
    !> bidi-inhibit-bpa.
    logical, public :: bidi_inhibit_bpa = .false.

    !> How many states are added to the cache at a time, and at most in
    !> one slot of the iterator stack (bidi.c).
    integer(int64), parameter :: BIDI_CACHE_CHUNK = 200, BIDI_CACHE_MAX_ELTS_PER_SLOT = 50000
    !> dispextern.h IT_STACK_SIZE.
    integer, parameter :: IT_STACK_SIZE = 5

    !> What the iterator remembers of a character (dispextern.h struct
    !> bidi_saved_info).
    type, public :: bidi_saved_info_type
        integer(int64) :: charpos = 0
        integer :: type = UNKNOWN_BT, orig_type = UNKNOWN_BT
    end type bidi_saved_info_type

    !> One level of the directional status stack (dispextern.h struct
    !> bidi_stack); `flags' holds the isolate status (bit 0), the override
    !> (bits 1-2) and the sos of an isolate (bit 3).
    type, public :: bidi_stack_type
        integer(int64) :: next_for_neutral_pos = 0
        integer :: next_for_neutral_type = 0, last_strong_type = 0, prev_for_neutral_type = 0
        integer :: level = 0, flags = 0
    end type bidi_stack_type

    !> The string being reordered (dispextern.h struct bidi_string_data):
    !> none when not `active' (the current buffer is reordered).
    type, public :: bidi_string_type
        logical :: active = .false.
        character(len=:), allocatable :: s
        integer(int64) :: schars = 0
    end type bidi_string_type

    !> The bidi iterator (dispextern.h struct bidi_it).  The components
    !> from `level_stack' on (but its levels in use) are not part of the
    !> state that is saved, cached, pushed or popped.
    type, public :: bidi_it_type
        integer(int64) :: bytepos = 0, charpos = 0
        integer(int32) :: ch = 0
        integer(int64) :: nchars = 0, ch_len = 0
        integer :: type = UNKNOWN_BT, type_after_wn = UNKNOWN_BT, orig_type = UNKNOWN_BT
        integer :: resolved_level = 0, isolate_level = 0
        integer(int64) :: invalid_levels = 0, invalid_isolates = 0
        type(bidi_saved_info_type) :: prev, last_strong, next_for_neutral, prev_for_neutral, next_for_ws
        integer(int64) :: bracket_pairing_pos = 0
        integer :: bracket_enclosed_type = UNKNOWN_BT
        integer(int64) :: next_en_pos = 0
        integer :: next_en_type = UNKNOWN_BT
        integer :: sos = L2R, scan_dir = 0
        integer(int64) :: disp_pos = 0
        integer :: disp_prop = 0
        integer :: stack_idx = 0
        type(bidi_stack_type) :: level_stack(0:BIDI_MAXDEPTH + 2)
        type(bidi_string_type) :: string
        integer :: paragraph_dir = NEUTRAL_DIR
        integer(int64) :: separator_limit = 0
        logical :: first_elt = .false., new_paragraph = .false., frame_window_p = .false.
    end type bidi_it_type

    !> The cache's state, saved by `bidi_shelve_cache'.
    type, public :: bidi_shelved_type
        logical :: empty = .true.
        type(bidi_it_type), allocatable :: cache(:)
        integer(int64) :: index = 0, start = 0, last_idx = 0, max_elts = 0
        integer(int64) :: start_stack(0:IT_STACK_SIZE - 1) = 0
        integer :: sp = 0
    end type bidi_shelved_type

    !> The depth of the bracket stack of `bidi_find_bracket_pairs': as
    !> many entries as fit upstream in MAX_ALLOCA (16 KiB) less two
    !> iterators, on a 64-bit system (bidi.c MAX_BPA_STACK).
    integer, parameter :: MAX_BPA_STACK = 977

    !> The furthest find_first_strong_char looks, and paragraph start
    !> searches (bidi.c MAX_STRONG_CHAR_SEARCH, MAX_PARAGRAPH_SEARCH).
    integer(int64), parameter :: MAX_STRONG_CHAR_SEARCH = 100000, MAX_PARAGRAPH_SEARCH = 7500

    !> Bracket stack flags (bidi.c).
    integer, parameter :: FLAG_EMBEDDING_INSIDE = 1, FLAG_OPPOSITE_INSIDE = 2

    !> The cache of iterator states (bidi.c bidi_cache and its indices).
    type(bidi_it_type), allocatable :: bidi_cache(:)
    integer(int64) :: bidi_cache_size = 0, bidi_cache_idx = 0, bidi_cache_last_idx = -1, bidi_cache_start = 0
    integer(int64) :: bidi_cache_max_elts = BIDI_CACHE_MAX_ELTS_PER_SLOT
    integer(int64) :: bidi_cache_start_stack(0:IT_STACK_SIZE - 1) = 0
    integer :: bidi_cache_sp = 0
    !> The characters find_first_strong_char examined (bidi.c).
    integer(int64) :: nsearch_for_strong = 0

contains

    ! ------------------------------------------------------------------
    ! Character properties
    ! ------------------------------------------------------------------

    !> The bidi class of character C (the bidi-class property table;
    !> L above U+10FFFF).  Not a port.
    integer function bidi_class(c) result(t)
        integer(int32), intent(in) :: c
        integer :: lo, hi, mid

        t = STRONG_L
        if (c > bidi_ends(bidi_runs)) return
        lo = 1
        hi = bidi_runs

        do while (lo < hi)
            mid = (lo + hi)/2

            if (bidi_ends(mid) < c) then
                lo = mid + 1
            else
                hi = mid
            end if
        end do

        t = bidi_codes(lo)
    end function bidi_class

    !> The index of C in the sorted array A, or 0.  Not a port.
    pure integer function search_sorted(a, c) result(k)
        integer(int32), intent(in) :: a(:), c
        integer :: lo, hi, mid

        k = 0
        lo = 1
        hi = size(a)

        do while (lo <= hi)
            mid = (lo + hi)/2

            if (a(mid) == c) then
                k = mid
                return
            else if (a(mid) < c) then
                lo = mid + 1
            else
                hi = mid - 1
            end if
        end do
    end function search_sorted

    !> The bidi type of character CH, with OVERRIDE (L2R or R2L) making
    !> it strong unless it is a formatting character.  Port of
    !> bidi.c:bidi_get_type.
    integer function bidi_get_type(ch, override) result(t)
        integer(int32), intent(in) :: ch
        integer, intent(in) :: override

        if (ch == BIDI_EOB) then
            t = NEUTRAL_B
            return
        end if

        t = bidi_class(ch)

        select case (t)
        case (WEAK_BN, NEUTRAL_B, LRE, LRO, RLE, RLO, PDF, LRI, RLI, FSI, PDI)
            continue
        case default
            if (override == L2R) then
                t = STRONG_L
            else if (override == R2L) then
                t = STRONG_R
            end if
        end select
    end function bidi_get_type

    !> The category of bidi type TYPE.  Port of bidi.c:bidi_get_category
    !> (with bidi.c:bidi_check_type: types are always valid here).
    pure integer function bidi_get_category(type) result(c)
        integer, intent(in) :: type

        select case (type)
        case (STRONG_L, STRONG_R, STRONG_AL)
            c = STRONG
        case (WEAK_EN, WEAK_ES, WEAK_ET, WEAK_AN, WEAK_CS, WEAK_NSM, WEAK_BN)
            c = WEAK
        case (NEUTRAL_B, NEUTRAL_S, NEUTRAL_WS, NEUTRAL_ON)
            c = NEUTRAL
        case (LRE, LRO, RLE, RLO, PDF, LRI, RLI, FSI, PDI)
            c = EXPLICIT_FORMATTING
        case default
            c = UNKNOWN_BC
        end select
    end function bidi_get_category

    !> Whether TYPE is an isolate formatting character.  Port of
    !> bidi.c:bidi_isolate_fmt_char.
    pure logical function bidi_isolate_fmt_char(type)
        integer, intent(in) :: type

        bidi_isolate_fmt_char = type == LRI .or. type == RLI .or. type == PDI .or. type == FSI
    end function bidi_isolate_fmt_char

    !> The mirror image of character C (C itself if it has none).  Port of
    !> bidi.c:bidi_mirror_char.
    integer(int32) function bidi_mirror_char(c) result(m)
        integer(int32), intent(in) :: c
        integer :: k

        m = c
        if (c == BIDI_EOB) return
        k = search_sorted(mirror_from, c)
        if (k > 0) m = mirror_to(k)
    end function bidi_mirror_char

    !> The paired bracket type of character C.  Port of
    !> bidi.c:bidi_paired_bracket_type.
    integer function bidi_paired_bracket_type(c) result(t)
        integer(int32), intent(in) :: c

        t = BIDI_BRACKET_NONE
        if (c == BIDI_EOB .or. bidi_inhibit_bpa) return

        if (search_sorted(open_brackets, c) > 0) then
            t = BIDI_BRACKET_OPEN
        else if (search_sorted(close_brackets, c) > 0) then
            t = BIDI_BRACKET_CLOSE
        end if
    end function bidi_paired_bracket_type

    !> Whether CH is an explicit embedding or override character (LRE,
    !> LRO, RLE, RLO or PDF).  Port of bidi.c:bidi_explicit_dir_char.
    logical function bidi_explicit_dir_char(ch)
        integer(int32), intent(in) :: ch
        integer :: t

        bidi_explicit_dir_char = .false.
        if (ch < 0) return
        t = bidi_class(ch)
        bidi_explicit_dir_char = t == LRE .or. t == LRO .or. t == RLE .or. t == RLO .or. t == PDF
    end function bidi_explicit_dir_char

    ! ------------------------------------------------------------------
    ! The iterator state
    ! ------------------------------------------------------------------

    !> Set the start-of-sequence type from the levels before and after a
    !> level run, and forget the characters before it.  Port of
    !> bidi.c:bidi_set_sos_type.
    subroutine bidi_set_sos_type(it, level_before, level_after)
        type(bidi_it_type), intent(inout) :: it
        integer, intent(in) :: level_before, level_after

        if (iand(max(level_before, level_after), 1) /= 0) then
            it%sos = R2L
        else
            it%sos = L2R
        end if

        it%prev%type = UNKNOWN_BT
        it%last_strong%type = UNKNOWN_BT
        it%last_strong%orig_type = UNKNOWN_BT

        if (it%sos == R2L) then
            it%prev_for_neutral%type = STRONG_R
        else
            it%prev_for_neutral%type = STRONG_L
        end if

        it%prev_for_neutral%charpos = it%charpos
        it%next_for_neutral%type = UNKNOWN_BT
        it%next_for_neutral%orig_type = UNKNOWN_BT
    end subroutine bidi_set_sos_type

    !> The isolate status of stack entry INDEX (bidi.c ISOLATE_STATUS).
    !> Not a port.
    pure logical function isolate_status(it, index)
        type(bidi_it_type), intent(in) :: it
        integer, intent(in) :: index

        isolate_status = iand(it%level_stack(index)%flags, 1) /= 0
    end function isolate_status

    !> Whether the character the iterator moved to starts a level run
    !> excluded from the isolating run sequence at CURRENT_LEVEL (an
    !> isolate, the stack having gone from OLD_SIDX to NEW_SIDX): a
    !> condition of bidi_find_bracket_pairs and bidi_resolve_neutral.
    !> Not a port.
    pure logical function excluded_run(it, old_sidx, new_sidx, current_level) result(excluded)
        type(bidi_it_type), intent(in) :: it
        integer, intent(in) :: old_sidx, new_sidx, current_level

        excluded = .false.
        if (it%level_stack(new_sidx)%level <= current_level) return
        excluded = isolate_status(it, new_sidx)
        if (excluded .or. new_sidx <= old_sidx + 1) return
        excluded = isolate_status(it, new_sidx - 1)
    end function excluded_run

    !> The override of stack entry INDEX (bidi.c OVERRIDE).  Not a port.
    pure integer function override_of(it, index)
        type(bidi_it_type), intent(in) :: it
        integer, intent(in) :: index

        override_of = iand(ishft(it%level_stack(index)%flags, -1), 3)
    end function override_of

    !> Push embedding level LEVEL with OVERRIDE and ISOLATE on the
    !> directional status stack.  Port of bidi.c:bidi_push_embedding_level.
    subroutine bidi_push_embedding_level(it, level, override, isolate)
        type(bidi_it_type), intent(inout) :: it
        integer, intent(in) :: level, override
        logical, intent(in) :: isolate
        integer :: prev_level

        prev_level = it%level_stack(it%stack_idx)%level
        it%stack_idx = it%stack_idx + 1

        associate (st => it%level_stack(it%stack_idx))
            st%level = level
            st%flags = ishft(iand(override, 3), 1)

            if (isolate) then
                st%flags = ior(st%flags, 1)
                st%last_strong_type = it%last_strong%type
                st%prev_for_neutral_type = it%prev_for_neutral%type
                st%next_for_neutral_type = it%next_for_neutral%type
                st%next_for_neutral_pos = it%next_for_neutral%charpos
                if (it%sos /= L2R) st%flags = ior(st%flags, 8)
            end if
        end associate

        call bidi_set_sos_type(it, prev_level, level)
    end subroutine bidi_push_embedding_level

    !> Pop the directional status stack and return the new embedding
    !> level.  Port of bidi.c:bidi_pop_embedding_level.
    integer function bidi_pop_embedding_level(it) result(level)
        type(bidi_it_type), intent(inout) :: it
        type(bidi_stack_type) :: st
        integer :: old_level

        if (it%stack_idx > 0) then
            old_level = it%level_stack(it%stack_idx)%level
            st = it%level_stack(it%stack_idx)

            if (isolate_status(it, it%stack_idx)) then
                it%prev%type = UNKNOWN_BT
                it%prev%orig_type = UNKNOWN_BT
                it%last_strong%type = st%last_strong_type
                it%prev_for_neutral%type = st%prev_for_neutral_type
                it%next_for_neutral%type = st%next_for_neutral_type
                it%next_for_neutral%charpos = st%next_for_neutral_pos

                if (iand(ishft(st%flags, -3), 1) == 0) then
                    it%sos = L2R
                else
                    it%sos = R2L
                end if
            else
                call bidi_set_sos_type(it, old_level, it%level_stack(it%stack_idx - 1)%level)
            end if

            it%stack_idx = it%stack_idx - 1
        end if

        level = it%level_stack(it%stack_idx)%level
    end function bidi_pop_embedding_level

    !> Remember the character the iterator is at in SAVED, with its
    !> resolved type if FROM_TYPE, else its type after the Wn rules.
    !> Port of bidi.c:bidi_remember_char.
    pure subroutine bidi_remember_char(saved, it, from_type)
        type(bidi_saved_info_type), intent(out) :: saved
        type(bidi_it_type), intent(in) :: it
        logical, intent(in) :: from_type

        saved%charpos = it%charpos

        if (from_type) then
            saved%type = it%type
        else
            saved%type = it%type_after_wn
        end if

        saved%orig_type = it%orig_type
    end subroutine bidi_remember_char

    !> Copy the state of iterator FROM to TO: everything before the
    !> stack, and the stack up to its top.  Port of bidi.c:bidi_copy_it.
    pure subroutine bidi_copy_it(to, from)
        type(bidi_it_type), intent(inout) :: to
        type(bidi_it_type), intent(in) :: from

        to%bytepos = from%bytepos
        to%charpos = from%charpos
        to%ch = from%ch
        to%nchars = from%nchars
        to%ch_len = from%ch_len
        to%type = from%type
        to%type_after_wn = from%type_after_wn
        to%orig_type = from%orig_type
        to%resolved_level = from%resolved_level
        to%isolate_level = from%isolate_level
        to%invalid_levels = from%invalid_levels
        to%invalid_isolates = from%invalid_isolates
        to%prev = from%prev
        to%last_strong = from%last_strong
        to%next_for_neutral = from%next_for_neutral
        to%prev_for_neutral = from%prev_for_neutral
        to%next_for_ws = from%next_for_ws
        to%bracket_pairing_pos = from%bracket_pairing_pos
        to%bracket_enclosed_type = from%bracket_enclosed_type
        to%next_en_pos = from%next_en_pos
        to%next_en_type = from%next_en_type
        to%sos = from%sos
        to%scan_dir = from%scan_dir
        to%disp_pos = from%disp_pos
        to%disp_prop = from%disp_prop
        to%stack_idx = from%stack_idx
        to%level_stack(0:from%stack_idx) = from%level_stack(0:from%stack_idx)
    end subroutine bidi_copy_it

    !> Whether the iterator is at the paragraph's embedding level
    !> (dispextern.h BIDI_AT_BASE_LEVEL).  Not a port.
    pure logical function bidi_at_base_level(it)
        type(bidi_it_type), intent(in) :: it

        bidi_at_base_level = it%resolved_level == it%level_stack(0)%level
    end function bidi_at_base_level

    ! ------------------------------------------------------------------
    ! The cache of iterator states
    ! ------------------------------------------------------------------

    !> Forget the cached states from the N-th of the current slot on.
    !> Port of bidi.c:bidi_cache_reset_to.
    subroutine bidi_cache_reset_to(n)
        integer(int64), intent(in) :: n

        bidi_cache_idx = bidi_cache_start + n
        bidi_cache_last_idx = -1
    end subroutine bidi_cache_reset_to

    !> Forget the cached states of the current slot.  Port of
    !> bidi.c:bidi_cache_reset.
    subroutine bidi_cache_reset()
        call bidi_cache_reset_to(0_int64)
    end subroutine bidi_cache_reset

    !> Give the cache back its first chunk and empty it.  Port of
    !> bidi.c:bidi_cache_shrink.
    subroutine bidi_cache_shrink()
        type(bidi_it_type), allocatable :: smaller(:)

        if (bidi_cache_size > BIDI_CACHE_CHUNK) then
            allocate (smaller(0:BIDI_CACHE_CHUNK - 1))
            smaller(0:BIDI_CACHE_CHUNK - 1) = bidi_cache(0:BIDI_CACHE_CHUNK - 1)
            call move_alloc(smaller, bidi_cache)
            bidi_cache_size = BIDI_CACHE_CHUNK
        end if

        call bidi_cache_reset()
        bidi_cache_max_elts = BIDI_CACHE_MAX_ELTS_PER_SLOT
    end subroutine bidi_cache_shrink

    !> Load the cached state INDEX into the iterator, keeping its scan
    !> direction.  Port of bidi.c:bidi_cache_fetch_state.
    subroutine bidi_cache_fetch_state(index, it)
        integer(int64), intent(in) :: index
        type(bidi_it_type), intent(inout) :: it
        integer :: current_scan_dir

        current_scan_dir = it%scan_dir
        call bidi_copy_it(it, bidi_cache(index))
        it%scan_dir = current_scan_dir
        bidi_cache_last_idx = index
    end subroutine bidi_cache_fetch_state

    !> The index of the cached state that covers CHARPOS at level LEVEL
    !> or lower (any level if -1), searching in direction DIR from the
    !> last state used, or -1.  Port of bidi.c:bidi_cache_search.
    integer(int64) function bidi_cache_search(charpos, level, dir_in) result(found)
        integer(int64), intent(in) :: charpos
        integer, intent(in) :: level, dir_in
        integer(int64) :: i, i_start
        integer :: dir

        found = -1
        dir = dir_in
        if (bidi_cache_idx <= bidi_cache_start) return
        if (bidi_cache_last_idx == -1) bidi_cache_last_idx = bidi_cache_idx - 1

        if (charpos < bidi_cache(bidi_cache_last_idx)%charpos) then
            dir = -1
            i_start = bidi_cache_last_idx - 1
        else if (charpos > bidi_cache(bidi_cache_last_idx)%charpos + bidi_cache(bidi_cache_last_idx)%nchars - 1) then
            dir = 1
            i_start = bidi_cache_last_idx + 1
        else if (dir /= 0) then
            i_start = bidi_cache_last_idx
        else
            dir = -1
            i_start = bidi_cache_idx - 1
        end if

        if (dir < 0) then
            do i = i_start, bidi_cache_start, -1
                if (covers(i)) then
                    found = i
                    return
                end if
            end do
        else
            do i = i_start, bidi_cache_idx - 1
                if (covers(i)) then
                    found = i
                    return
                end if
            end do
        end if

    contains

        !> Whether state I covers CHARPOS at a level LEVEL allows.
        logical function covers(i)
            integer(int64), intent(in) :: i

            covers = .false.
            if (bidi_cache(i)%charpos > charpos) return
            if (charpos >= bidi_cache(i)%charpos + bidi_cache(i)%nchars) return
            covers = level == -1 .or. bidi_cache(i)%resolved_level <= level
        end function covers
    end function bidi_cache_search

    !> The index of the cached state where the level drops below LEVEL,
    !> searching from the last state used in direction DIR (from the end
    !> if 0); with BEFORE, the state before that change.  -1 if none.
    !> Port of bidi.c:bidi_cache_find_level_change.
    integer(int64) function bidi_cache_find_level_change(level, dir_in, before) result(found)
        integer, intent(in) :: level, dir_in
        logical, intent(in) :: before
        integer(int64) :: i
        integer :: incr, dir

        found = -1
        if (bidi_cache_idx == 0) return
        dir = dir_in

        if (dir /= 0) then
            i = bidi_cache_last_idx
        else
            i = bidi_cache_idx - 1
        end if

        incr = 0
        if (before) incr = 1
        if (i < 0) i = 0

        if (dir == 0) then
            dir = -1
        else if (incr == 0) then
            i = i + dir
        end if

        if (dir < 0) then
            do while (i >= bidi_cache_start + incr)
                if (bidi_cache(i - incr)%resolved_level >= 0 .and. bidi_cache(i - incr)%resolved_level < level) then
                    found = i
                    return
                end if

                i = i - 1
            end do
        else
            do while (i < bidi_cache_idx - incr)
                if (bidi_cache(i + incr)%resolved_level >= 0 .and. bidi_cache(i + incr)%resolved_level < level) then
                    found = i
                    return
                end if

                i = i + 1
            end do
        end if
    end function bidi_cache_find_level_change

    !> The position where the run of characters at LEVEL or higher that
    !> ends at the last cached state starts, or -1.  Port of
    !> bidi.c:bidi_level_start.
    integer(int64) function bidi_level_start(level) result(pos)
        integer, intent(in) :: level
        integer(int64) :: slot

        pos = -1
        slot = bidi_cache_find_level_change(level, -1, .true.)
        if (slot >= 0) pos = bidi_cache(slot)%charpos
    end function bidi_level_start

    !> Make room in the cache for the state at index INDEX, if the slot
    !> allows that many.  Port of bidi.c:bidi_cache_ensure_space.
    subroutine bidi_cache_ensure_space(index)
        integer(int64), intent(in) :: index
        type(bidi_it_type), allocatable :: grown(:)
        integer(int64) :: chunk_size, new_size

        if (index < bidi_cache_size) return
        chunk_size = BIDI_CACHE_CHUNK
        if (bidi_cache_size > bidi_cache_max_elts - chunk_size) chunk_size = bidi_cache_max_elts - bidi_cache_size
        if (max(index + 1, bidi_cache_size + chunk_size) > bidi_cache_max_elts) return
        new_size = min(bidi_cache_max_elts, bidi_cache_size + max(chunk_size, index - bidi_cache_size + 1))
        allocate (grown(0:new_size - 1))
        if (bidi_cache_size > 0) grown(0:bidi_cache_size - 1) = bidi_cache(0:bidi_cache_size - 1)
        call move_alloc(grown, bidi_cache)
        bidi_cache_size = new_size
    end subroutine bidi_cache_ensure_space

    !> Cache the iterator's state (as unresolved unless RESOLVED); with
    !> UPDATE_ONLY, only update a state already cached.  Return whether it
    !> was cached.  Port of bidi.c:bidi_cache_iterator_state.
    logical function bidi_cache_iterator_state(it, resolved, update_only) result(cached)
        type(bidi_it_type), intent(in) :: it
        logical, intent(in) :: resolved, update_only
        integer(int64) :: index

        cached = .false.
        index = bidi_cache_search(it%charpos, -1, 1)
        if (index < 0 .and. update_only) return

        if (index < 0) then
            index = bidi_cache_idx
            call bidi_cache_ensure_space(index)

            ! Don't let the cache grow out of contiguity.
            if (bidi_cache_start < index .and. index < bidi_cache_size) then
                if (it%charpos > bidi_cache(index - 1)%charpos + bidi_cache(index - 1)%nchars .or. &
                    it%charpos < bidi_cache(bidi_cache_start)%charpos) then
                    call bidi_cache_reset()
                    index = bidi_cache_start
                end if
            end if

            if (bidi_cache_size > index) then
                call bidi_copy_it(bidi_cache(index), it)
                if (.not. resolved) bidi_cache(index)%resolved_level = -1
            end if
        else
            ! Copy only the members that could have changed.
            bidi_cache(index)%type = it%type
            bidi_cache(index)%type_after_wn = it%type_after_wn

            if (resolved) then
                bidi_cache(index)%resolved_level = it%resolved_level
            else
                bidi_cache(index)%resolved_level = -1
            end if

            bidi_cache(index)%invalid_levels = it%invalid_levels
            bidi_cache(index)%next_for_neutral = it%next_for_neutral
            bidi_cache(index)%next_for_ws = it%next_for_ws
            bidi_cache(index)%disp_pos = it%disp_pos
            bidi_cache(index)%disp_prop = it%disp_prop
            bidi_cache(index)%bracket_pairing_pos = it%bracket_pairing_pos
            bidi_cache(index)%bracket_enclosed_type = it%bracket_enclosed_type
        end if

        if (bidi_cache_size > index) then
            bidi_cache_last_idx = index
            if (index >= bidi_cache_idx) bidi_cache_idx = index + 1
            cached = .true.
        else
            bidi_cache_last_idx = -1
        end if
    end function bidi_cache_iterator_state

    !> Load the cached state at CHARPOS (only a resolved one if
    !> RESOLVED_ONLY) into the iterator and return its type, or
    !> UNKNOWN_BT.  Port of bidi.c:bidi_cache_find.
    integer function bidi_cache_find(charpos, resolved_only, it) result(type)
        integer(int64), intent(in) :: charpos
        logical, intent(in) :: resolved_only
        type(bidi_it_type), intent(inout) :: it
        integer(int64) :: i
        integer :: current_scan_dir

        type = UNKNOWN_BT
        i = bidi_cache_search(charpos, -1, it%scan_dir)
        if (i < bidi_cache_start) return
        if (resolved_only .and. bidi_cache(i)%resolved_level < 0) return
        current_scan_dir = it%scan_dir
        call bidi_copy_it(it, bidi_cache(i))
        bidi_cache_last_idx = i
        it%scan_dir = current_scan_dir
        type = it%type
    end function bidi_cache_find

    !> The level of the next character in the scan direction, from the
    !> cache.  Port of bidi.c:bidi_peek_at_next_level.
    integer function bidi_peek_at_next_level(it) result(level)
        type(bidi_it_type), intent(in) :: it

        if (bidi_cache_last_idx == -1 .or. (bidi_cache_last_idx >= bidi_cache_idx - 1 .and. it%scan_dir > 0)) then
            level = bidi_cache(bidi_cache_idx - 1)%resolved_level
        else
            level = bidi_cache(bidi_cache_last_idx + it%scan_dir)%resolved_level
        end if
    end function bidi_peek_at_next_level

    !> Push the iterator's state on the cache and start a new slot (for
    !> reordering a string within the buffer text).  Port of
    !> bidi.c:bidi_push_it.
    subroutine bidi_push_it(it)
        type(bidi_it_type), intent(in) :: it

        bidi_cache_max_elts = bidi_cache_max_elts + BIDI_CACHE_MAX_ELTS_PER_SLOT
        call bidi_cache_ensure_space(bidi_cache_idx)
        bidi_cache(bidi_cache_idx) = it
        bidi_cache_idx = bidi_cache_idx + 1
        bidi_cache_start_stack(bidi_cache_sp) = bidi_cache_start
        bidi_cache_sp = bidi_cache_sp + 1
        bidi_cache_start = bidi_cache_idx
        bidi_cache_last_idx = -1
    end subroutine bidi_push_it

    !> Restore the state `bidi_push_it' saved, and its cache slot.
    !> Port of bidi.c:bidi_pop_it.
    subroutine bidi_pop_it(it)
        type(bidi_it_type), intent(inout) :: it

        bidi_cache_idx = bidi_cache_start - 1
        it = bidi_cache(bidi_cache_idx)
        bidi_cache_sp = bidi_cache_sp - 1
        bidi_cache_start = bidi_cache_start_stack(bidi_cache_sp)
        bidi_cache_last_idx = -1
        bidi_cache_max_elts = bidi_cache_max_elts - BIDI_CACHE_MAX_ELTS_PER_SLOT
    end subroutine bidi_pop_it

    !> Save the cache, to restore it with `bidi_unshelve_cache' after an
    !> unrelated iteration.  Port of bidi.c:bidi_shelve_cache.
    function bidi_shelve_cache() result(data)
        type(bidi_shelved_type) :: data

        if (bidi_cache_idx == 0) return
        data%empty = .false.
        allocate (data%cache(0:bidi_cache_idx - 1))
        data%cache(0:bidi_cache_idx - 1) = bidi_cache(0:bidi_cache_idx - 1)
        data%index = bidi_cache_idx
        data%start_stack = bidi_cache_start_stack
        data%sp = bidi_cache_sp
        data%start = bidi_cache_start
        data%last_idx = bidi_cache_last_idx
        data%max_elts = bidi_cache_max_elts
    end function bidi_shelve_cache

    !> Restore the cache DATA saved (only free it with JUST_FREE); an empty
    !> DATA resets the cache.  Port of bidi.c:bidi_unshelve_cache.
    subroutine bidi_unshelve_cache(data, just_free)
        type(bidi_shelved_type), intent(inout) :: data
        logical, intent(in) :: just_free

        if (data%empty) then
            if (.not. just_free) then
                bidi_cache_start = 0
                bidi_cache_sp = 0
                bidi_cache_max_elts = BIDI_CACHE_MAX_ELTS_PER_SLOT
                call bidi_cache_reset()
            end if

            return
        end if

        if (.not. just_free) then
            bidi_cache_idx = data%index
            call bidi_cache_ensure_space(bidi_cache_idx)
            bidi_cache(0:bidi_cache_idx - 1) = data%cache(0:bidi_cache_idx - 1)
            bidi_cache_start_stack = data%start_stack
            bidi_cache_sp = data%sp
            bidi_cache_start = data%start
            bidi_cache_last_idx = data%last_idx
            bidi_cache_max_elts = data%max_elts
        end if

        if (allocated(data%cache)) deallocate (data%cache)
        data%empty = .true.
    end subroutine bidi_unshelve_cache

    ! ------------------------------------------------------------------
    ! Initialization
    ! ------------------------------------------------------------------

    !> Reorder the string S (UTF-8, multibyte) with iterator IT instead of
    !> the current buffer; without S, the buffer again.  Not a port (the
    !> `string' member of struct bidi_it).
    subroutine bidi_set_string(it, s)
        type(bidi_it_type), intent(inout) :: it
        character(len=*), intent(in), optional :: s
        integer(int64) :: k
        integer(int32) :: ch, n

        it%string%active = present(s)
        it%string%schars = 0
        if (allocated(it%string%s)) deallocate (it%string%s)
        if (.not. present(s)) return
        it%string%s = s
        k = 1

        do while (k <= len(s, kind=int64))
            call string_char_and_length(s, k, ch, n)
            k = k + n
            it%string%schars = it%string%schars + 1
        end do
    end subroutine bidi_set_string

    !> Reset the iterator's state for the end of a paragraph.  Port of
    !> bidi.c:bidi_set_paragraph_end.
    subroutine bidi_set_paragraph_end(it)
        type(bidi_it_type), intent(inout) :: it

        it%invalid_levels = 0
        it%invalid_isolates = 0
        it%stack_idx = 0
        it%isolate_level = 0
        it%resolved_level = it%level_stack(0)%level
    end subroutine bidi_set_paragraph_end

    !> Set up IT to reorder from CHARPOS and BYTEPOS (unchanged where
    !> negative).  Port of bidi.c:bidi_init_it (with
    !> bidi.c:bidi_initialize: the tables are compiled in).
    subroutine bidi_init_it(charpos, bytepos, frame_window_p, it)
        integer(int64), intent(in) :: charpos, bytepos
        logical, intent(in) :: frame_window_p
        type(bidi_it_type), intent(inout) :: it

        if (charpos >= 0) it%charpos = charpos
        if (bytepos >= 0) it%bytepos = bytepos
        it%frame_window_p = frame_window_p
        ! This means "need to recompute".
        it%nchars = -1
        it%first_elt = .true.
        call bidi_set_paragraph_end(it)
        it%new_paragraph = .true.
        it%separator_limit = -1
        it%type = NEUTRAL_B
        it%type_after_wn = NEUTRAL_B
        it%orig_type = NEUTRAL_B
        it%prev%type = UNKNOWN_BT
        it%prev%orig_type = UNKNOWN_BT
        it%last_strong%type = UNKNOWN_BT
        it%last_strong%orig_type = UNKNOWN_BT
        it%next_for_neutral%charpos = -1
        it%next_for_neutral%type = UNKNOWN_BT
        it%next_for_neutral%orig_type = UNKNOWN_BT
        it%prev_for_neutral%charpos = -1
        it%prev_for_neutral%type = UNKNOWN_BT
        it%prev_for_neutral%orig_type = UNKNOWN_BT
        it%bracket_pairing_pos = -1
        it%sos = L2R
        it%disp_pos = -1
        it%disp_prop = 0

        if (bidi_cache_start == 0) then
            call bidi_cache_shrink()
        else
            call bidi_cache_reset()
        end if
    end subroutine bidi_init_it

    !> Set up the iterator for the start of a line.  Port of
    !> bidi.c:bidi_line_init.
    subroutine bidi_line_init(it)
        type(bidi_it_type), intent(inout) :: it
        integer :: before

        it%scan_dir = 1
        it%stack_idx = 0
        it%resolved_level = it%level_stack(0)%level
        it%level_stack(0)%flags = 0
        it%invalid_levels = 0
        it%isolate_level = 0
        it%invalid_isolates = 0
        it%next_en_pos = 0
        it%next_en_type = UNKNOWN_BT
        it%next_for_ws%charpos = -1
        it%next_for_ws%type = UNKNOWN_BT
        it%bracket_pairing_pos = -1
        before = 0
        if (it%paragraph_dir == R2L) before = 1
        call bidi_set_sos_type(it, before, it%level_stack(0)%level)
        call bidi_cache_reset()
    end subroutine bidi_line_init

    ! ------------------------------------------------------------------
    ! Fetching characters
    ! ------------------------------------------------------------------

    !> The end of the text: the string's length, or ZV.  Not a port.
    integer(int64) function text_end(it) result(e)
        type(bidi_it_type), intent(in) :: it

        if (it%string%active) then
            e = it%string%schars
        else
            e = buffers(current)%zv
        end if
    end function text_end

    !> The character at BYTEPOS of the text and its length in bytes.
    !> Port of bidi.c:bidi_char_at_pos (with bidi.c:bidi_count_bytes for
    !> the length).
    subroutine bidi_char_at_pos(it, bytepos, ch, len)
        type(bidi_it_type), intent(in) :: it
        integer(int64), intent(in) :: bytepos
        integer(int32), intent(out) :: ch
        integer(int64), intent(out) :: len
        integer(int32) :: n

        if (it%string%active) then
            call string_char_and_length(it%string%s, bytepos + 1, ch, n)
            len = n
        else
            ch = buf_fetch_char(buffers(current), bytepos)
            len = buf_next_char_len(buffers(current), bytepos)
        end if
    end subroutine bidi_char_at_pos

    !> The character at CHARPOS and BYTEPOS, BIDI_EOB at the end, with
    !> its length in bytes and characters.  Port of bidi.c:bidi_fetch_char
    !> (display properties are Deferred: DISP_POS is the end of the text).
    integer(int32) function bidi_fetch_char(it, charpos, bytepos, disp_pos, disp_prop, ch_len, nchars) result(ch)
        type(bidi_it_type), intent(in) :: it
        integer(int64), intent(in) :: charpos, bytepos
        integer(int64), intent(inout) :: disp_pos
        integer, intent(inout) :: disp_prop
        integer(int64), intent(out) :: ch_len, nchars
        integer(int64) :: endpos

        endpos = text_end(it)
        disp_pos = endpos
        disp_prop = 0
        nchars = 1

        if (charpos >= endpos) then
            ch = BIDI_EOB
            ch_len = 1
        else
            call bidi_char_at_pos(it, bytepos, ch, ch_len)
        end if
    end function bidi_fetch_char

    !> Like `bidi_fetch_char', but an isolate initiator comes with the
    !> isolated text up to its matching PDI, as one character.  Port of
    !> bidi.c:bidi_fetch_char_skip_isolates.
    integer(int32) function bidi_fetch_char_skip_isolates(it, charpos_in, bytepos_in, disp_pos, disp_prop, ch_len, &
                                                          nchars) result(ch)
        type(bidi_it_type), intent(in) :: it
        integer(int64), intent(in) :: charpos_in, bytepos_in
        integer(int64), intent(inout) :: disp_pos
        integer, intent(inout) :: disp_prop
        integer(int64), intent(out) :: ch_len, nchars
        integer(int64) :: charpos, bytepos, level
        integer :: ch_type

        charpos = charpos_in
        bytepos = bytepos_in
        ch = bidi_fetch_char(it, charpos, bytepos, disp_pos, disp_prop, ch_len, nchars)
        ch_type = bidi_get_type(ch, NEUTRAL_DIR)
        level = 0

        if (ch_type == LRI .or. ch_type == RLI .or. ch_type == FSI) then
            level = level + 1

            do while (level > 0 .and. ch_type /= NEUTRAL_B)
                charpos = charpos + nchars
                bytepos = bytepos + ch_len
                ch = bidi_fetch_char(it, charpos, bytepos, disp_pos, disp_prop, ch_len, nchars)
                ch_type = bidi_get_type(ch, NEUTRAL_DIR)

                if (ch_type == LRI .or. ch_type == RLI .or. ch_type == FSI) then
                    level = level + 1
                else if (ch_type == PDI) then
                    level = level - 1
                end if
            end do
        end if

        ! Make sure the data for the isolate is all of it.
        nchars = nchars + charpos - charpos_in
        ch_len = ch_len + bytepos - bytepos_in
    end function bidi_fetch_char_skip_isolates

    ! ------------------------------------------------------------------
    ! Paragraphs
    ! ------------------------------------------------------------------

    !> The regexps of a paragraph separator and start (bidi.c
    !> paragraph_separate_regexp, paragraph_start_regexp).  Not a port.
    function paragraph_regexp(start) result(regexp)
        logical, intent(in) :: start
        character(len=:), allocatable :: regexp

        if (start) then
            regexp = "^\("//achar(12)//"\|[ "//achar(9)//"]*\)$"
        else
            regexp = "^[ "//achar(9)//achar(12)//"]*$"
        end if
    end function paragraph_regexp

    !> The length of the paragraph separator at CHARPOS (0 for an empty
    !> line), -1 if a paragraph starts there without a separator, -2 if
    !> neither.  Port of bidi.c:bidi_at_paragraph_end.
    integer(int64) function bidi_at_paragraph_end(charpos) result(val)
        integer(int64), intent(in) :: charpos
        type(status_type) :: st

        val = fast_looking_at(paragraph_regexp(.false.), charpos, buffers(current)%zv, st)

        if (val < 0) then
            if (fast_looking_at(paragraph_regexp(.true.), charpos, buffers(current)%zv, st) >= 0) then
                val = -1
            else
                val = -2
            end if
        end if
    end function bidi_at_paragraph_end

    !> The byte position of the start of the paragraph holding POS (the
    !> line where a paragraph start matches, at most MAX_PARAGRAPH_SEARCH
    !> lines back, else BEGV).  Port of bidi.c:bidi_find_paragraph_start
    !> (without the region cache of bidi_paragraph_cache_on_off, Deferred).
    integer(int64) function bidi_find_paragraph_start(pos_in, pos_byte_in) result(pos_byte)
        integer(int64), intent(in) :: pos_in, pos_byte_in
        integer(int64) :: pos, n
        type(status_type) :: st

        pos = pos_in
        pos_byte = pos_byte_in
        n = 0

        do while (pos_byte > buffers(current)%begv_byte)
            n = n + 1
            if (n > MAX_PARAGRAPH_SEARCH) exit
            if (fast_looking_at(paragraph_regexp(.true.), pos, buffers(current)%zv, st) >= 0) exit
            ! dec_both
            pos = pos - 1
            pos_byte = pos_byte - buf_prev_char_len(buffers(current), pos_byte)
            pos = find_newline_no_quit(pos, pos_byte, -1_int64, pos_byte)
        end do

        if (n >= MAX_PARAGRAPH_SEARCH) pos_byte = buffers(current)%begv_byte
        pos_byte = max(buffers(current)%begv_byte, min(pos_byte, buffers(current)%zv_byte))
    end function bidi_find_paragraph_start

    !> The type of the first strong character from POS (or NEUTRAL_B at a
    !> paragraph's end, or PDI with STOP_AT_PDI), isolates skipped.
    !> Port of bidi.c:find_first_strong_char.
    integer function find_first_strong_char(it, pos_in, bytepos_in, end, disp_pos, disp_prop, ch_len, nchars, &
                                            stop_at_pdi) result(type)
        type(bidi_it_type), intent(in) :: it
        integer(int64), intent(in) :: pos_in, bytepos_in, end
        integer(int64), intent(inout) :: disp_pos
        integer, intent(inout) :: disp_prop
        integer(int64), intent(inout) :: ch_len, nchars
        logical, intent(in) :: stop_at_pdi
        integer(int64) :: pos, bytepos, pos1
        integer(int32) :: ch

        pos = pos_in
        bytepos = bytepos_in

        if (stop_at_pdi) then
            ! Skip the FSI itself.
            pos = pos + nchars
            bytepos = bytepos + ch_len
        end if

        ch = bidi_fetch_char_skip_isolates(it, pos, bytepos, disp_pos, disp_prop, ch_len, nchars)
        type = bidi_get_type(ch, NEUTRAL_DIR)
        pos1 = pos
        pos = pos + nchars
        bytepos = bytepos + ch_len

        do while (bidi_get_category(type) /= STRONG .and. .not. (stop_at_pdi .and. type == PDI) .and. &
                  pos - pos1 < MAX_STRONG_CHAR_SEARCH)
            if (pos >= end) then
                ! Pretend there's a paragraph separator at end of buffer.
                type = NEUTRAL_B
                exit
            end if

            if (.not. it%string%active .and. type == NEUTRAL_B) then
                if (bidi_at_paragraph_end(pos) >= -1) exit
            end if

            ch = bidi_fetch_char_skip_isolates(it, pos, bytepos, disp_pos, disp_prop, ch_len, nchars)
            pos = pos + nchars
            bytepos = bytepos + ch_len
            type = bidi_get_type(ch, NEUTRAL_DIR)
        end do

        nsearch_for_strong = nsearch_for_strong + pos - pos1
    end function find_first_strong_char

    !> Determine the base direction of the paragraph at the iterator: DIR
    !> if L2R or R2L, else from its first strong character (with
    !> NO_DEFAULT_P, that of an earlier paragraph if it has none), and
    !> start a line.  Port of bidi.c:bidi_paragraph_init.
    subroutine bidi_paragraph_init(dir_in, it, no_default_p)
        integer, intent(in) :: dir_in
        type(bidi_it_type), intent(inout) :: it
        logical, intent(in) :: no_default_p
        integer(int64) :: bytepos, pstartbyte, begbyte, end, pos, ch_len, nchars, disp_pos, prevpbyte, p, pbyte
        integer(int64) :: len
        integer(int32) :: ch
        integer :: dir, type, disp_prop

        bytepos = it%bytepos
        begbyte = buffers(current)%begv_byte
        end = buffers(current)%zv

        if (it%string%active) then
            begbyte = 0
            end = it%string%schars
        end if

        pos = it%charpos
        nsearch_for_strong = 0
        dir = dir_in
        ! Special case for an empty buffer.
        if (bytepos == begbyte .and. it%charpos == end) dir = L2R

        if (dir == L2R) then
            it%paragraph_dir = L2R
            it%new_paragraph = .false.
        else if (dir == R2L) then
            it%paragraph_dir = R2L
            it%new_paragraph = .false.
        else
            ! Use the default heuristics: the first strong character.
            disp_pos = -1
            disp_prop = 0
            if (.not. it%first_elt .and. it%charpos < it%separator_limit) return
            pos = it%charpos

            if (bytepos > begbyte) then
                call bidi_char_at_pos(it, bytepos, ch, len)

                if (ch == 10) then
                    bytepos = bytepos + 1
                    pos = pos + 1
                end if
            end if

            if (it%string%active) then
                pstartbyte = 0
            else
                pstartbyte = bidi_find_paragraph_start(pos, bytepos)
            end if

            it%separator_limit = -1
            it%new_paragraph = .false.

            do
                bytepos = pstartbyte
                if (.not. it%string%active) pos = byte_to_char(bytepos)
                ch_len = 0
                nchars = 0
                type = find_first_strong_char(it, pos, bytepos, end, disp_pos, disp_prop, ch_len, nchars, .false.)

                if (type == STRONG_R .or. type == STRONG_AL) then
                    it%paragraph_dir = R2L
                else if (type == STRONG_L) then
                    it%paragraph_dir = L2R
                end if

                if (.not. it%string%active .and. no_default_p .and. it%paragraph_dir == NEUTRAL_DIR) then
                    ! Search back for a paragraph with a strong character.
                    if (pstartbyte == buffers(current)%begv_byte) then
                        it%paragraph_dir = L2R
                    else
                        prevpbyte = pstartbyte
                        pbyte = pstartbyte
                        p = byte_to_char(pstartbyte)

                        do while (pbyte > buffers(current)%begv_byte .and. prevpbyte >= pstartbyte)
                            p = p - 1
                            pbyte = pbyte - buf_prev_char_len(buffers(current), pbyte)
                            prevpbyte = bidi_find_paragraph_start(p, pbyte)
                        end do

                        pstartbyte = prevpbyte
                    end if
                end if

                if (it%string%active .or. .not. no_default_p .or. it%paragraph_dir /= NEUTRAL_DIR) exit
            end do
        end if

        ! Contrary to UAX#9 clause P3, we only default the paragraph
        ! direction to L2R if we have no previous usable paragraph.
        if (it%paragraph_dir /= L2R .and. it%paragraph_dir /= R2L) it%paragraph_dir = L2R

        if (it%paragraph_dir == R2L) then
            it%level_stack(0)%level = 1
        else
            it%level_stack(0)%level = 0
        end if

        call bidi_line_init(it)
    end subroutine bidi_paragraph_init

    ! ------------------------------------------------------------------
    ! Resolving the levels
    ! ------------------------------------------------------------------

    !> Move the iterator to the next character and resolve its explicit
    !> embedding level (rules X1 to X8 of UAX#9); return it.  Port of
    !> bidi.c:bidi_resolve_explicit.
    integer function bidi_resolve_explicit(it) result(level)
        type(bidi_it_type), intent(inout) :: it
        integer(int32) :: curchar
        integer :: type, typ1, prev_type, current_level, new_level, override, disp_prop
        integer :: stack_override
        logical :: isolate
        integer(int64) :: ch_len, nchars, disp_pos, end, eob, begbyte

        prev_type = UNKNOWN_BT
        eob = text_end(it)

        if (it%type_after_wn /= WEAK_BN .and. it%type /= WEAK_BN) then
            ! A closing bracket resolved to a strong type by the BPA is
            ! remembered with that type (N0).
            if (it%type_after_wn == NEUTRAL_ON .and. bidi_get_category(it%type) == STRONG .and. &
                bidi_paired_bracket_type(it%ch) == BIDI_BRACKET_CLOSE) then
                call bidi_remember_char(it%prev, it, .true.)
            else
                call bidi_remember_char(it%prev, it, .false.)
            end if
        end if

        if (it%type_after_wn == STRONG_R .or. it%type_after_wn == STRONG_L .or. it%type_after_wn == STRONG_AL) then
            call bidi_remember_char(it%last_strong, it, .false.)
        end if

        if (it%type == STRONG_R .or. it%type == STRONG_L .or. it%type == WEAK_EN .or. it%type == WEAK_AN) then
            call bidi_remember_char(it%prev_for_neutral, it, .true.)
        end if

        ! If we overstepped the characters used for resolving neutrals and
        ! whitespace, invalidate their info.
        if (it%charpos >= it%next_for_neutral%charpos) then
            it%next_for_neutral%type = UNKNOWN_BT
            if (it%bracket_pairing_pos == eob) it%bracket_pairing_pos = -1
        end if

        if (it%next_en_pos >= 0 .and. it%charpos >= it%next_en_pos) then
            it%next_en_pos = 0
            it%next_en_type = UNKNOWN_BT
        end if

        ! Reset the bracket resolution info, unless the BPA was found
        ! unnecessary for this isolating run sequence.
        if (it%bracket_pairing_pos /= eob) then
            it%bracket_pairing_pos = -1
            it%bracket_enclosed_type = UNKNOWN_BT
        end if

        begbyte = buffers(current)%begv_byte
        if (it%string%active) begbyte = 0

        if (it%bytepos < begbyte .or. it%first_elt) then
            it%first_elt = .false.

            if (it%string%active) then
                if (it%charpos < 0) then
                    it%charpos = 0
                    it%bytepos = 0
                end if
            else if (it%charpos < buffers(current)%begv) then
                it%charpos = buffers(current)%begv
                it%bytepos = buffers(current)%begv_byte
            end if

            ! The previous character, if any, is a newline (bidi_line_init).
            if (it%first_elt .and. it%prev%type /= UNKNOWN_BT) prev_type = it%prev%orig_type
        else if (it%charpos < eob) then
            it%charpos = it%charpos + it%nchars
            it%bytepos = it%bytepos + it%ch_len
            prev_type = it%orig_type
        else
            ! At the end of the text, the previous type is NEUTRAL_B.
            prev_type = NEUTRAL_B
        end if

        current_level = it%level_stack(it%stack_idx)%level
        isolate = isolate_status(it, it%stack_idx)
        override = override_of(it, it%stack_idx)
        new_level = current_level

        if (it%charpos >= eob) then
            curchar = BIDI_EOB
            it%ch_len = 1
            it%nchars = 1
            it%disp_pos = eob
            it%disp_prop = 0
        else
            ! The level of an isolate initiator or a PDF changes after it.
            select case (prev_type)
            case (RLI)
                if (current_level < BIDI_MAXDEPTH .and. it%invalid_levels == 0 .and. it%invalid_isolates == 0) then
                    new_level = iand(current_level + 1, not(1)) + 1
                    it%isolate_level = it%isolate_level + 1
                    call bidi_push_embedding_level(it, new_level, NEUTRAL_DIR, .true.)
                else
                    it%invalid_isolates = it%invalid_isolates + 1
                end if
            case (LRI)
                if (current_level < BIDI_MAXDEPTH - 1 .and. it%invalid_levels == 0 .and. it%invalid_isolates == 0) then
                    new_level = iand(current_level + 2, not(1))
                    it%isolate_level = it%isolate_level + 1
                    call bidi_push_embedding_level(it, new_level, NEUTRAL_DIR, .true.)
                else
                    it%invalid_isolates = it%invalid_isolates + 1
                end if
            case (PDF)
                if (it%invalid_isolates == 0) then
                    if (it%invalid_levels > 0) then
                        it%invalid_levels = it%invalid_levels - 1
                    else if (.not. isolate .and. it%stack_idx >= 1) then
                        new_level = bidi_pop_embedding_level(it)
                    end if
                end if
            case default
                continue
            end select

            curchar = bidi_fetch_char(it, it%charpos, it%bytepos, it%disp_pos, it%disp_prop, it%ch_len, it%nchars)
        end if

        it%ch = curchar
        it%resolved_level = new_level
        ! Don't apply directional override here, as all the types we handle
        ! below will not be affected by the override anyway.
        type = bidi_get_type(curchar, NEUTRAL_DIR)
        it%orig_type = type
        it%type_after_wn = UNKNOWN_BT

        select case (type)
        case (RLE, RLO)
            it%type_after_wn = type
            ! X9/Retaining
            type = WEAK_BN

            if (new_level < BIDI_MAXDEPTH .and. it%invalid_levels == 0 .and. it%invalid_isolates == 0) then
                new_level = iand(new_level + 1, not(1)) + 1

                if (it%type_after_wn == RLE) then
                    override = NEUTRAL_DIR
                else
                    override = R2L
                end if

                call bidi_push_embedding_level(it, new_level, override, .false.)
                it%resolved_level = new_level
            else if (it%invalid_isolates == 0) then
                it%invalid_levels = it%invalid_levels + 1
            end if
        case (LRE, LRO)
            it%type_after_wn = type
            type = WEAK_BN

            if (new_level < BIDI_MAXDEPTH - 1 .and. it%invalid_levels == 0 .and. it%invalid_isolates == 0) then
                new_level = iand(new_level + 2, not(1))

                if (it%type_after_wn == LRE) then
                    override = NEUTRAL_DIR
                else
                    override = L2R
                end if

                call bidi_push_embedding_level(it, new_level, override, .false.)
                it%resolved_level = new_level
            else if (it%invalid_isolates == 0) then
                it%invalid_levels = it%invalid_levels + 1
            end if
        case (FSI, RLI, LRI)
            if (type == FSI) then
                ! X5c: an FSI is an RLI if its first strong character is R
                ! or AL, else an LRI.
                end = eob
                disp_pos = it%disp_pos
                disp_prop = it%disp_prop
                nchars = it%nchars
                ch_len = it%ch_len
                typ1 = find_first_strong_char(it, it%charpos, it%bytepos, end, disp_pos, disp_prop, ch_len, nchars, &
                                              .true.)

                if (typ1 /= STRONG_R .and. typ1 /= STRONG_AL) then
                    type = LRI
                else
                    type = RLI
                end if

                it%orig_type = type
            end if

            if (override == NEUTRAL_DIR) then
                it%type_after_wn = type
            else if (override == L2R) then
                ! An isolate initiator is affected by the override.
                it%type_after_wn = STRONG_L
            else
                it%type_after_wn = STRONG_R
            end if
        case (PDI)
            if (it%invalid_isolates > 0) then
                it%invalid_isolates = it%invalid_isolates - 1
            else if (it%isolate_level > 0) then
                it%invalid_levels = 0

                do while (.not. isolate_status(it, it%stack_idx))
                    new_level = bidi_pop_embedding_level(it)
                end do

                new_level = bidi_pop_embedding_level(it)
                it%isolate_level = it%isolate_level - 1
            end if

            it%resolved_level = new_level
            ! Unlike the other formatting characters, PDI is affected by
            ! the directional override of the enclosing level.
            stack_override = override_of(it, it%stack_idx)

            if (stack_override == L2R) then
                it%type_after_wn = STRONG_L
            else if (stack_override == R2L) then
                it%type_after_wn = STRONG_R
            else
                it%type_after_wn = type
            end if
        case (PDF)
            it%type_after_wn = type
            type = WEAK_BN
        case default
            continue
        end select

        it%type = type

        if (it%type == NEUTRAL_B) then
            ! X8: at a paragraph end, all explicit levels end.
            call bidi_set_paragraph_end(it)
            it%type_after_wn = it%type
        end if

        level = it%resolved_level
    end function bidi_resolve_explicit

    !> Resolve the weak types of the next character (rules W1 to W7 of
    !> UAX#9).  Port of bidi.c:bidi_resolve_weak.
    integer function bidi_resolve_weak(it) result(type)
        type(bidi_it_type), intent(inout) :: it
        integer :: override, prev_level, new_level, type_of_next
        integer(int32) :: next_char
        integer(int64) :: eob, en_pos, len
        type(bidi_it_type) :: saved_it

        prev_level = it%level_stack(it%stack_idx)%level
        new_level = bidi_resolve_explicit(it)
        eob = text_end(it)
        type = it%type
        override = override_of(it, it%stack_idx)
        ! Reset sos at a paragraph separator.
        if (it%type == NEUTRAL_B) call bidi_set_sos_type(it, prev_level, new_level)

        if (type == NEUTRAL_S .or. type == NEUTRAL_WS .or. type == WEAK_BN .or. type == STRONG_AL) then
            ! Not changed by the Wn rules.
            it%type_after_wn = type
        end if

        if (override == R2L) then
            type = STRONG_R
        else if (override == L2R) then
            type = STRONG_L
        else
            if (type == WEAK_NSM) then
                ! W1
                if (it%prev%type /= UNKNOWN_BT .and. it%prev%type /= NEUTRAL_B) then
                    if (bidi_isolate_fmt_char(it%prev%type)) then
                        type = NEUTRAL_ON
                    else
                        type = it%prev%type
                    end if
                else if (it%sos == R2L) then
                    type = STRONG_R
                else
                    type = STRONG_L
                end if
            end if

            if (type == WEAK_EN .and. it%last_strong%type == STRONG_AL) then
                ! W2
                type = WEAK_AN
            else if (type == STRONG_AL) then
                ! W3
                type = STRONG_R
            else if ((type == WEAK_ES .and. it%prev%type == WEAK_EN .and. it%prev%orig_type == WEAK_EN) .or. &
                     (type == WEAK_CS .and. ((it%prev%type == WEAK_EN .and. it%prev%orig_type == WEAK_EN) .or. &
                                             it%prev%type == WEAK_AN))) then
                ! W4
                if (it%charpos + it%nchars >= eob) then
                    next_char = BIDI_EOB
                else
                    call bidi_char_at_pos(it, it%bytepos + it%ch_len, next_char, len)
                end if

                type_of_next = bidi_get_type(next_char, override)

                if (type_of_next == WEAK_BN .or. bidi_explicit_dir_char(next_char)) then
                    call bidi_copy_it(saved_it, it)

                    do while (bidi_resolve_explicit(it) == new_level .and. it%type == WEAK_BN)
                        type_of_next = it%type
                    end do

                    call bidi_copy_it(it, saved_it)
                end if

                if (type == WEAK_ES .and. type_of_next == WEAK_EN .and. it%last_strong%type /= STRONG_AL) then
                    type = WEAK_EN
                else if (type == WEAK_CS) then
                    if (it%prev%type == WEAK_AN .and. (type_of_next == WEAK_AN .or. &
                                                       (type_of_next == WEAK_EN .and. it%last_strong%type == STRONG_AL))) &
                        then
                        type = WEAK_AN
                    else if (it%prev%type == WEAK_EN .and. type_of_next == WEAK_EN .and. &
                             it%last_strong%type /= STRONG_AL) then
                        type = WEAK_EN
                    end if
                end if
            else if (type == WEAK_ET .or. type == WEAK_BN) then
                ! W5 and BN that is part of a sequence of ETs
                if (it%prev%type == WEAK_EN) then
                    type = WEAK_EN
                else if (it%next_en_pos > it%charpos .and. it%next_en_type /= WEAK_BN) then
                    if (it%next_en_type == WEAK_EN) type = WEAK_EN
                else if (type == WEAK_BN .and. new_level == 0 .and. .not. bidi_explicit_dir_char(it%ch) .and. &
                         ((it%last_strong%type == STRONG_L) .or. &
                          (it%last_strong%type == UNKNOWN_BT .and. it%sos == L2R))) then
                    ! A BN at level 0 after L stays L.
                    type = STRONG_L
                else if (it%next_en_pos >= 0) then
                    ! Look for the first character that is not ET or BN.
                    en_pos = it%charpos + it%nchars

                    if (it%charpos + it%nchars >= eob) then
                        next_char = BIDI_EOB
                    else
                        call bidi_char_at_pos(it, it%bytepos + it%ch_len, next_char, len)
                    end if

                    type_of_next = bidi_get_type(next_char, override)

                    if (type_of_next == WEAK_ET .or. type_of_next == WEAK_BN .or. bidi_explicit_dir_char(next_char)) then
                        call bidi_copy_it(saved_it, it)

                        do while (bidi_resolve_explicit(it) == new_level .and. &
                                  (it%type == WEAK_BN .or. it%type == WEAK_ET))
                            type_of_next = it%type
                        end do

                        if (type == WEAK_BN .and. it%charpos == saved_it%charpos + saved_it%nchars) then
                            ! A BN followed by a non-ET, non-BN.
                            en_pos = saved_it%charpos
                            type_of_next = type
                        else
                            en_pos = it%charpos
                        end if

                        call bidi_copy_it(it, saved_it)
                    end if

                    ! Remember this position, to speed up processing of
                    ! the next ETs.
                    it%next_en_pos = en_pos

                    if (type_of_next == WEAK_EN) then
                        if (it%last_strong%type == STRONG_AL) then
                            type_of_next = WEAK_AN
                        else if (type == WEAK_BN) then
                            ! A BN followed by EN: it becomes ON, so that
                            ! the later N1 rule resolves it.
                            type = NEUTRAL_ON
                        else
                            type = WEAK_EN
                        end if
                    else if (type_of_next == NEUTRAL_B) then
                        ! Don't save the position of a paragraph end.
                        it%next_en_pos = -1
                    end if

                    it%next_en_type = type_of_next
                end if
            end if
        end if

        ! W6
        if (type == WEAK_ES .or. type == WEAK_ET .or. type == WEAK_CS .or. &
            (type == WEAK_BN .and. (it%prev%type == WEAK_CS .or. it%prev%type == WEAK_ES .or. &
                                    it%prev%type == WEAK_ET))) type = NEUTRAL_ON
        ! Store the type we've got so far, before we clobber it with
        ! strong types in W7 and while resolving neutral types.
        if (it%type_after_wn == UNKNOWN_BT) it%type_after_wn = type

        ! W7
        if (type == WEAK_EN) then
            if (it%last_strong%type == STRONG_L .or. (it%last_strong%type == UNKNOWN_BT .and. it%sos == L2R)) then
                type = STRONG_L
            end if
        end if

        it%type = type
    end function bidi_resolve_weak

    !> The type of a neutral between PREV_TYPE and NEXT_TYPE at level LEV
    !> (rules N1 and N2 of UAX#9).  Port of bidi.c:bidi_resolve_neutral_1.
    pure integer function bidi_resolve_neutral_1(prev_type_in, next_type_in, lev) result(type)
        integer, intent(in) :: prev_type_in, next_type_in, lev
        integer :: prev_type, next_type

        prev_type = prev_type_in
        next_type = next_type_in
        ! N1: European and Arabic numbers act as if they were R.
        if (next_type == WEAK_EN .or. next_type == WEAK_AN) next_type = STRONG_R
        if (prev_type == WEAK_EN .or. prev_type == WEAK_AN) prev_type = STRONG_R

        if (next_type == prev_type) then
            type = next_type
        else if (iand(lev, 1) == 0) then
            ! N2: the embedding direction.
            type = STRONG_L
        else
            type = STRONG_R
        end if
    end function bidi_resolve_neutral_1

    !> The closing bracket of character C's canonical equivalent
    !> (bidi.c CANONICAL_EQU: U+2329 and U+232A are U+3008 and U+3009).
    !> Not a port.
    integer(int32) function canonical_equ(c) result(e)
        integer(int32), intent(in) :: c

        e = c
        if (c == int(z'2329', int32)) e = int(z'3008', int32)
        if (c == int(z'232A', int32)) e = int(z'3009', int32)
    end function canonical_equ

    !> At an opening bracket, find its closing pair in the isolating run
    !> sequence, caching the states on the way, and record in the cache
    !> the type of what the pairs enclose (rules BD16 and N0 of UAX#9).
    !> Return whether a pair was found.  Port of
    !> bidi.c:bidi_find_bracket_pairs.
    logical function bidi_find_bracket_pairs(it) result(retval)
        type(bidi_it_type), intent(inout) :: it
        integer :: btype, type, bpa_sp, base_level, embedding_level, maxlevel, embedding_type, sp, flag
        integer :: old_sidx, new_sidx, current_level
        integer(int32) :: close_char(0:MAX_BPA_STACK - 1), curchar
        integer(int64) :: open_index(0:MAX_BPA_STACK - 1), pairing_pos, idx_at_entry, eob
        integer :: flags(0:MAX_BPA_STACK - 1)
        type(bidi_it_type) :: saved_it, tem_it
        logical :: l2r_seen, r2l_seen, ok

        retval = .false.
        type = it%type
        btype = bidi_paired_bracket_type(it%ch)
        if (btype /= BIDI_BRACKET_OPEN) return
        bpa_sp = -1
        base_level = it%level_stack(0)%level
        embedding_level = it%level_stack(it%stack_idx)%level
        maxlevel = embedding_level

        if (iand(embedding_level, 1) /= 0) then
            embedding_type = STRONG_R
        else
            embedding_type = STRONG_L
        end if

        l2r_seen = .false.
        r2l_seen = .false.
        pairing_pos = -1
        idx_at_entry = bidi_cache_idx
        call bidi_copy_it(saved_it, it)
        ! The iterator for fetching cached states is always forward.
        tem_it%scan_dir = 1

        do
            current_level = it%level_stack(it%stack_idx)%level
            if (maxlevel < current_level) maxlevel = current_level
            ! Mark the opening bracket as being resolved (bracket_pairing_pos).
            if (btype == BIDI_BRACKET_OPEN .and. it%bracket_pairing_pos == -1) it%bracket_pairing_pos = it%charpos

            if (.not. bidi_cache_iterator_state(it, type == NEUTRAL_B, .false.)) then
                ! No room in the cache: give up on the BPA.
                call bidi_cache_reset_to(idx_at_entry - bidi_cache_start)
                call bidi_copy_it(it, saved_it)
                return
            end if

            if (btype == BIDI_BRACKET_OPEN) then
                ! PUSH_BPA_STACK
                if (bpa_sp < MAX_BPA_STACK - 1) then
                    bpa_sp = bpa_sp + 1
                    close_char(bpa_sp) = bidi_mirror_char(canonical_equ(it%ch))
                    open_index(bpa_sp) = bidi_cache_last_idx
                    flags(bpa_sp) = 0
                end if
            else if (btype == BIDI_BRACKET_CLOSE) then
                sp = bpa_sp
                curchar = canonical_equ(it%ch)

                do while (sp >= 0)
                    if (close_char(sp) == curchar) exit
                    sp = sp - 1
                end do

                if (sp >= 0) then
                    ! Update the cached state of the opening bracket with
                    ! the type of what the pair encloses.
                    call bidi_cache_fetch_state(open_index(sp), tem_it)

                    if (iand(flags(sp), FLAG_EMBEDDING_INSIDE) /= 0) then
                        tem_it%bracket_enclosed_type = embedding_type
                    else if (iand(flags(sp), FLAG_OPPOSITE_INSIDE) /= 0) then
                        if (embedding_type == STRONG_L) then
                            tem_it%bracket_enclosed_type = STRONG_R
                        else
                            tem_it%bracket_enclosed_type = STRONG_L
                        end if
                    else
                        tem_it%bracket_enclosed_type = UNKNOWN_BT
                    end if

                    tem_it%bracket_pairing_pos = it%charpos
                    ok = bidi_cache_iterator_state(tem_it, .false., .true.)
                    ! Pop the BPA stack.
                    bpa_sp = sp - 1
                end if

                if (bpa_sp < 0) then
                    retval = .true.
                    exit
                end if
            else if (bidi_get_category(it%type_after_wn) /= NEUTRAL) then
                flag = 0

                select case (it%type)
                case (STRONG_L)
                    if (iand(embedding_level, 1) == 0) then
                        flag = FLAG_EMBEDDING_INSIDE
                    else
                        flag = FLAG_OPPOSITE_INSIDE
                    end if

                    l2r_seen = .true.
                case (STRONG_R, WEAK_EN, WEAK_AN)
                    if (iand(embedding_level, 1) == 1) then
                        flag = FLAG_EMBEDDING_INSIDE
                    else
                        flag = FLAG_OPPOSITE_INSIDE
                    end if

                    r2l_seen = .true.
                case default
                    continue
                end select

                if (flag /= 0) then
                    do sp = bpa_sp, 0, -1
                        flags(sp) = ior(flags(sp), flag)
                    end do
                end if
            end if

            old_sidx = it%stack_idx
            type = bidi_resolve_weak(it)
            new_sidx = it%stack_idx

            ! Skip level runs excluded from this isolating run sequence.
            if (excluded_run(it, old_sidx, new_sidx, current_level)) then
                do while (it%level_stack(it%stack_idx)%level > current_level)
                    if (maxlevel < it%level_stack(it%stack_idx)%level) maxlevel = it%level_stack(it%stack_idx)%level

                    if (.not. bidi_cache_iterator_state(it, type == NEUTRAL_B, .false.)) then
                        call bidi_cache_reset_to(idx_at_entry - bidi_cache_start)
                        call bidi_copy_it(it, saved_it)
                        return
                    end if

                    type = bidi_resolve_weak(it)
                end do
            end if

            if (type == NEUTRAL_B .or. it%level_stack(it%stack_idx)%level /= current_level) then
                ! We've marched all the way to the end of this isolating
                ! run sequence, and didn't find matching closing brackets.
                pairing_pos = it%charpos
                exit
            end if

            if (it%type_after_wn == NEUTRAL_ON) then
                btype = bidi_paired_bracket_type(it%ch)
            else
                btype = BIDI_BRACKET_NONE
            end if
        end do

        ! Restore bidi_it from the cache, which should have the bracket
        ! resolution members set as determined by the above loop.
        type = bidi_cache_find(saved_it%charpos, .false., it)

        ! The BPA is unnecessary if the sequence has only characters of
        ! the base direction at the base level.
        if (maxlevel == base_level .and. (l2r_seen .or. r2l_seen) .and. &
            ((base_level == 0 .and. .not. r2l_seen) .or. (base_level == 1 .and. .not. l2r_seen))) then
            eob = text_end(it)
            if (retval) pairing_pos = it%bracket_pairing_pos
            ! The special value EOB says the BPA was not needed.
            it%bracket_pairing_pos = eob
            it%bracket_enclosed_type = embedding_type
            call bidi_cache_reset_to(bidi_cache_last_idx - bidi_cache_start)
            it%next_for_neutral%type = embedding_type
            it%next_for_neutral%charpos = pairing_pos
            retval = .false.
        end if
    end function bidi_find_bracket_pairs

    !> Record INFO as the next (NEXTP) or previous character for resolving
    !> neutrals in the first cached state after the last used one at
    !> LEVEL.  Port of bidi.c:bidi_record_type_for_neutral.
    subroutine bidi_record_type_for_neutral(info, level, nextp)
        type(bidi_saved_info_type), intent(in) :: info
        integer, intent(in) :: level
        logical, intent(in) :: nextp
        integer(int64) :: index
        integer :: lev

        do index = bidi_cache_last_idx + 1, bidi_cache_idx - 1
            lev = bidi_cache(index)%level_stack(bidi_cache(index)%stack_idx)%level

            if (lev <= level) then
                if (nextp) then
                    bidi_cache(index)%next_for_neutral = info
                else
                    bidi_cache(index)%prev_for_neutral = info
                end if

                exit
            end if
        end do
    end subroutine bidi_record_type_for_neutral

    !> Resolve the type of the next character with the paired brackets
    !> (rule N0 of UAX#9).  Port of bidi.c:bidi_resolve_brackets.
    integer function bidi_resolve_brackets(it) result(type)
        type(bidi_it_type), intent(inout) :: it
        integer :: prev_level, embedding_level, embedding_type, prev_type_for_neutral
        logical :: resolve_bracket
        integer(int32) :: ch
        integer(int64) :: eob, index
        type(bidi_saved_info_type) :: prev_for_neutral, next_for_neutral

        prev_level = it%level_stack(it%stack_idx)%level
        resolve_bracket = .false.
        type = UNKNOWN_BT
        ch = 0
        eob = text_end(it)

        if (it%type == STRONG_L .or. it%type == STRONG_R .or. it%type == WEAK_AN .or. it%type == WEAK_EN) then
            call bidi_remember_char(prev_for_neutral, it, .true.)
        else
            prev_for_neutral = it%prev_for_neutral
        end if

        ! The next character for resolving neutrals, if still ahead.
        if (it%next_for_neutral%charpos > it%charpos) then
            next_for_neutral = it%next_for_neutral
        else
            next_for_neutral%charpos = -1
        end if

        if (.not. it%first_elt) then
            type = bidi_cache_find(it%charpos + it%nchars, .false., it)
            ch = it%ch
        end if

        if (type == UNKNOWN_BT) then
            type = bidi_resolve_weak(it)

            if (type == NEUTRAL_ON) then
                ! The BPA was found unnecessary for this sequence: a closing
                ! bracket takes the type of the run.
                if (it%bracket_pairing_pos == eob) then
                    if (it%next_for_neutral%charpos == it%charpos .and. &
                        bidi_paired_bracket_type(it%ch) == BIDI_BRACKET_CLOSE) type = it%bracket_enclosed_type
                else if (bidi_find_bracket_pairs(it)) then
                    resolve_bracket = .true.
                end if
            end if
        else if (it%bracket_pairing_pos /= eob) then
            ! The cached state of an isolate's first character carries its
            ! own neutral context.
            if (it%level_stack(it%stack_idx)%level > prev_level .and. isolate_status(it, it%stack_idx)) then
                call bidi_record_type_for_neutral(prev_for_neutral, prev_level, .false.)
                call bidi_record_type_for_neutral(next_for_neutral, prev_level, .true.)
            end if

            if (type == NEUTRAL_ON .and. bidi_paired_bracket_type(ch) == BIDI_BRACKET_OPEN) then
                if (it%bracket_pairing_pos > it%charpos) then
                    ! A cached opening bracket with a known pair.
                    resolve_bracket = .true.
                else if (it%bracket_pairing_pos == -1) then
                    ! Not yet resolved: look for its pair.
                    if (bidi_find_bracket_pairs(it)) resolve_bracket = .true.
                end if
            end if

            ! Keep the neutral context of the level run.
            if (it%level_stack(it%stack_idx)%level == prev_level) then
                it%prev_for_neutral = prev_for_neutral
                if (next_for_neutral%charpos > 0) it%next_for_neutral = next_for_neutral
            end if
        end if

        if (resolve_bracket) then
            embedding_level = it%level_stack(it%stack_idx)%level

            if (iand(embedding_level, 1) /= 0) then
                embedding_type = STRONG_R
            else
                embedding_type = STRONG_L
            end if

            if (it%bracket_enclosed_type == embedding_type) then
                ! N0b
                type = embedding_type
            else if (it%bracket_enclosed_type == STRONG_L .or. it%bracket_enclosed_type == STRONG_R) then
                ! N0c: the type before the brackets decides.
                prev_type_for_neutral = it%prev_for_neutral%type
                if (prev_type_for_neutral == UNKNOWN_BT) prev_type_for_neutral = embedding_type

                select case (prev_type_for_neutral)
                case (STRONG_R, WEAK_EN, WEAK_AN)
                    if (it%bracket_enclosed_type == STRONG_R) then
                        type = STRONG_R
                    else
                        type = embedding_type
                    end if
                case (STRONG_L)
                    if (it%bracket_enclosed_type == STRONG_L) then
                        type = STRONG_L
                    else
                        type = embedding_type
                    end if
                case default
                    continue
                end select
            end if

            ! The closing bracket gets the same type.
            if (type /= NEUTRAL_ON) then
                index = bidi_cache_search(it%bracket_pairing_pos, -1, 1)
                if (index >= bidi_cache_start) bidi_cache(index)%type = type
            end if
        end if
    end function bidi_resolve_brackets

    !> Resolve the neutral types of the next character (rules N1 and N2
    !> of UAX#9).  Port of bidi.c:bidi_resolve_neutral.
    integer function bidi_resolve_neutral(it) result(type)
        type(bidi_it_type), intent(inout) :: it
        integer :: current_level, next_type, old_sidx, new_sidx
        logical :: is_neutral, adjacent_to_neutrals, ok
        type(bidi_it_type) :: saved_it

        type = bidi_resolve_brackets(it)
        current_level = it%level_stack(it%stack_idx)%level
        is_neutral = bidi_get_category(type) == NEUTRAL

        if (.not. ((type /= NEUTRAL_B .and. (is_neutral .or. bidi_isolate_fmt_char(type))) .or. type == WEAK_BN)) then
            return
        end if

        if (it%next_for_neutral%type /= UNKNOWN_BT .and. &
            (it%next_for_neutral%charpos > it%charpos .or. &
             (it%next_for_neutral%charpos == it%charpos .and. it%type == PDI))) then
            ! The next strong character is already known.
            type = bidi_resolve_neutral_1(it%prev_for_neutral%type, it%next_for_neutral%type, current_level)
        else if (current_level == 0 .and. it%prev_for_neutral%type == STRONG_L .and. &
                 (it%ch >= 0 .and. it%ch < 128 .or. (type /= WEAK_BN .and. .not. bidi_explicit_dir_char(it%ch) .and. &
                                                     .not. bidi_isolate_fmt_char(type)))) then
            ! Speed up: neutrals after L at level 0 are L (as N1/N2 make
            ! them, whatever follows).
            type = bidi_resolve_neutral_1(it%prev_for_neutral%type, STRONG_L, current_level)
        else if (current_level == 1 .and. it%level_stack(0)%level == 1 .and. &
                 (it%prev_for_neutral%type == STRONG_R .or. it%prev_for_neutral%type == WEAK_EN .or. &
                  it%prev_for_neutral%type == WEAK_AN) .and. type /= WEAK_BN .and. &
                 .not. bidi_explicit_dir_char(it%ch) .and. .not. bidi_isolate_fmt_char(type)) then
            ! Likewise for neutrals after R at level 1 of an R2L paragraph.
            type = bidi_resolve_neutral_1(it%prev_for_neutral%type, STRONG_R, current_level)
        else
            ! Look ahead for the first strong character, caching the
            ! neutrals on the way.
            adjacent_to_neutrals = is_neutral
            call bidi_copy_it(saved_it, it)

            do
                ok = bidi_cache_iterator_state(it, type == NEUTRAL_B, .false.)
                old_sidx = it%stack_idx
                type = bidi_resolve_brackets(it)
                new_sidx = it%stack_idx

                ! Skip level runs excluded from this isolating run sequence.
                if (excluded_run(it, old_sidx, new_sidx, current_level)) then
                    do while (it%level_stack(it%stack_idx)%level > current_level)
                        ok = bidi_cache_iterator_state(it, type == NEUTRAL_B, .false.)
                        type = bidi_resolve_brackets(it)
                    end do
                end if

                if (.not. adjacent_to_neutrals .and. (bidi_get_category(type) == NEUTRAL .or. &
                                                      bidi_isolate_fmt_char(type))) adjacent_to_neutrals = .true.
                if (type == NEUTRAL_B) exit
                if (type /= WEAK_BN .and. bidi_get_category(type) /= NEUTRAL .and. .not. bidi_isolate_fmt_char(type)) exit
                if (it%level_stack(it%stack_idx)%level /= current_level) exit
            end do

            ! Record the character that stopped the search.
            call bidi_remember_char(saved_it%next_for_neutral, it, .true.)

            if (it%level_stack(it%stack_idx)%level /= current_level .or. type == NEUTRAL_B) then
                ! The sequence ended: the eos type is the embedding direction.
                if (adjacent_to_neutrals) then
                    next_type = it%prev_for_neutral%type
                else
                    ! A BN or an isolate initiator between strong
                    ! characters: leave it as it is.
                    call bidi_copy_it(it, saved_it)
                    type = it%type
                    return
                end if
            else
                select case (type)
                case (STRONG_L, STRONG_R, STRONG_AL)
                    next_type = type
                case default
                    ! WEAK_EN, WEAK_AN: N1, numbers act as R.
                    next_type = STRONG_R
                end select
            end if

            type = bidi_resolve_neutral_1(saved_it%prev_for_neutral%type, next_type, current_level)
            saved_it%next_for_neutral%type = next_type
            saved_it%type = type
            call bidi_copy_it(it, saved_it)
        end if
    end function bidi_resolve_neutral

    !> The resolved type of the next character.  Port of
    !> bidi.c:bidi_type_of_next_char.
    integer function bidi_type_of_next_char(it) result(type)
        type(bidi_it_type), intent(inout) :: it

        type = bidi_resolve_neutral(it)
    end function bidi_type_of_next_char

    !> Move the iterator to the next character in logical order (or in
    !> the scan direction through the cache) and return its resolved
    !> level (rules I1, I2 and L1 of UAX#9).  Port of
    !> bidi.c:bidi_level_of_next_char.
    integer function bidi_level_of_next_char(it) result(level)
        type(bidi_it_type), intent(inout) :: it
        integer :: type, chtype, dpp
        integer(int64) :: next_char_pos, bob, clen, bpos, cpos, disp_pos, nc
        integer(int32) :: ch
        logical :: ok

        type = UNKNOWN_BT
        next_char_pos = -2

        if (it%scan_dir == 1) then
            ! There's no sense in trying to advance if we've hit the end.
            if (it%charpos >= text_end(it)) then
                level = it%resolved_level
                return
            end if
        end if

        ! Perhaps the character is in the cache.
        if (bidi_cache_idx > bidi_cache_start .and. .not. it%first_elt) then
            bob = 1
            if (it%string%active) bob = 0

            if (it%scan_dir > 0) then
                next_char_pos = it%charpos + it%nchars
            else if (it%charpos >= bob) then
                ! Previous character is at EOB if charpos is 0 in a string.
                next_char_pos = it%charpos - 1
            end if

            if (next_char_pos >= bob - 1) type = bidi_cache_find(next_char_pos, .true., it)

            if (type /= UNKNOWN_BT) then
                level = it%resolved_level
                return
            end if
        end if

        if (type == UNKNOWN_BT) type = bidi_type_of_next_char(it)

        if (type == NEUTRAL_B) then
            level = it%resolved_level
            return
        end if

        level = it%level_stack(it%stack_idx)%level
        it%type = type

        ! L1: whitespace, BNs and isolate formatting characters before a
        ! segment or paragraph separator, or at the end of a line, get the
        ! paragraph level.  Find the character after them.
        if ((it%orig_type == NEUTRAL_WS .or. (it%orig_type == WEAK_BN .and. level /= it%level_stack(0)%level) .or. &
             bidi_isolate_fmt_char(it%orig_type)) .and. it%next_for_ws%charpos < it%charpos) then
            clen = it%ch_len
            bpos = it%bytepos
            cpos = it%charpos
            disp_pos = it%disp_pos
            nc = it%nchars
            dpp = it%disp_prop

            do
                cpos = cpos + nc
                bpos = bpos + clen
                ch = bidi_fetch_char(it, cpos, bpos, disp_pos, dpp, clen, nc)
                chtype = bidi_get_type(ch, NEUTRAL_DIR)
                if (.not. (chtype == NEUTRAL_WS .or. chtype == WEAK_BN .or. bidi_isolate_fmt_char(chtype) .or. &
                           bidi_explicit_dir_char(ch))) exit
            end do

            it%next_for_ws%type = chtype
            it%next_for_ws%charpos = cpos
        end if

        ! Update the cache, but only if this state was already cached.
        ok = bidi_cache_iterator_state(it, .true., .true.)

        ! Resolve implicit levels.
        if (it%orig_type == NEUTRAL_B .or. it%orig_type == NEUTRAL_S .or. it%ch == 10 .or. it%ch == BIDI_EOB .or. &
            ((it%orig_type == NEUTRAL_WS .or. it%orig_type == WEAK_BN .or. bidi_isolate_fmt_char(it%orig_type) .or. &
              bidi_explicit_dir_char(it%ch)) .and. &
             (it%next_for_ws%type == NEUTRAL_B .or. it%next_for_ws%type == NEUTRAL_S))) then
            level = it%level_stack(0)%level
        else if (iand(level, 1) == 0) then
            ! I1
            if (type == STRONG_R) then
                level = level + 1
            else if (type == WEAK_EN .or. type == WEAK_AN) then
                level = level + 2
            end if
        else
            ! I2
            if (type == STRONG_L .or. type == WEAK_EN .or. type == WEAK_AN) level = level + 1
        end if

        it%resolved_level = level
    end function bidi_level_of_next_char

    !> Move to the other edge of the run at LEVEL or higher: forward
    !> through new characters, or (END_FLAG) back through the cache.
    !> Port of bidi.c:bidi_find_other_level_edge.
    subroutine bidi_find_other_level_edge(it, level, end_flag)
        type(bidi_it_type), intent(inout) :: it
        integer, intent(in) :: level
        logical, intent(in) :: end_flag
        integer :: dir, new_level
        integer(int64) :: index
        logical :: ok

        if (end_flag) then
            dir = -it%scan_dir
        else
            dir = it%scan_dir
        end if

        index = bidi_cache_find_level_change(level, dir, end_flag)

        if (index >= bidi_cache_start) then
            call bidi_cache_fetch_state(index, it)
        else
            ok = bidi_cache_iterator_state(it, .true., .false.)

            do
                new_level = bidi_level_of_next_char(it)

                if (.not. bidi_cache_iterator_state(it, .true., .false.)) then
                    ! The cache is full: the rest is at the base level.
                    new_level = level - 1
                    call bidi_cache_fetch_state(bidi_cache_idx - 1, it)
                    it%resolved_level = new_level
                    ok = bidi_cache_iterator_state(it, .true., .true.)
                end if

                if (new_level < level) exit
            end do
        end if
    end subroutine bidi_find_other_level_edge

    !> Move the iterator to the next character in visual order (rule L2 of
    !> UAX#9: runs of higher levels are reversed).  Port of
    !> bidi.c:bidi_move_to_visually_next.
    subroutine bidi_move_to_visually_next(it)
        type(bidi_it_type), intent(inout) :: it
        integer :: old_level, new_level, next_level, level_to_search, incr, expected_next_level
        type(bidi_it_type) :: sentinel
        logical :: ascending, ok
        integer(int64) :: sep_len

        if (it%scan_dir == 0) it%scan_dir = 1
        ! If we just passed a newline, initialize for the next line.
        if (.not. it%first_elt .and. (it%ch == 10 .or. it%ch == BIDI_EOB)) call bidi_line_init(it)

        ! Prepare the sentinel iterator state: a newline before the start.
        if (bidi_cache_idx == bidi_cache_start) then
            call bidi_copy_it(sentinel, it)

            if (it%first_elt) then
                sentinel%charpos = sentinel%charpos - 1
                sentinel%bytepos = sentinel%bytepos - 1
                sentinel%ch = 10
                sentinel%ch_len = 1
                sentinel%nchars = 1
            end if

            ok = bidi_cache_iterator_state(sentinel, .true., .false.)
        end if

        old_level = it%resolved_level
        new_level = bidi_level_of_next_char(it)

        ! Reordering of resolved levels (clause L2) is implemented by
        ! jumping to the other edge of the level and flipping direction.
        if (new_level /= old_level) then
            ascending = new_level > old_level

            if (ascending) then
                level_to_search = old_level + 1
                incr = 1
            else
                level_to_search = old_level
                incr = -1
            end if

            expected_next_level = old_level + incr
            call bidi_find_other_level_edge(it, level_to_search,.not. ascending)
            it%scan_dir = -it%scan_dir
            next_level = bidi_peek_at_next_level(it)

            do while (next_level /= expected_next_level)
                expected_next_level = expected_next_level + incr
                level_to_search = level_to_search + incr
                call bidi_find_other_level_edge(it, level_to_search,.not. ascending)
                it%scan_dir = -it%scan_dir
                next_level = bidi_peek_at_next_level(it)
            end do

            ! Finally, deliver the next character in the new direction.
            next_level = bidi_level_of_next_char(it)
        end if

        ! Take note when we have just processed the newline that precedes
        ! the end of the paragraph.
        if (it%scan_dir == 1 .and. (it%ch == 10 .or. it%ch == BIDI_EOB)) then
            if (it%string%active) then
                it%separator_limit = it%string%schars
            else if (it%bytepos < buffers(current)%zv_byte) then
                sep_len = bidi_at_paragraph_end(it%charpos + it%nchars)

                if (sep_len >= 0) then
                    it%new_paragraph = .true.

                    ! Record the buffer position of the last character of
                    ! the paragraph separator.
                    if (sep_len > 0) then
                        it%separator_limit = it%charpos + it%nchars + sep_len
                    else
                        it%separator_limit = it%charpos
                    end if
                end if
            end if
        end if

        ! The cache is no longer needed when back at the base level after
        ! the cached characters.
        if (it%scan_dir == 1 .and. bidi_cache_idx > bidi_cache_start) then
            if (it%resolved_level == it%level_stack(0)%level .and. &
                it%charpos > bidi_cache(bidi_cache_idx - 1)%charpos + bidi_cache(bidi_cache_idx - 1)%nchars - 1) then
                call bidi_cache_reset()
                ! hazard-ok(short-circuit): bidi_cache_idx > bidi_cache_start >= 0 here, so the index is valid.
            else if (it%resolved_level == it%level_stack(0)%level .and. bidi_cache_idx >= bidi_cache_size .and. &
                     it%charpos == bidi_cache(bidi_cache_idx - 1)%charpos) then
                call bidi_cache_reset()
            else
                ok = bidi_cache_iterator_state(it, .true., .false.)
            end if
        end if
    end subroutine bidi_move_to_visually_next

    !> The position of the first character up to the end of the line
    !> whose directionality an override or embedding changed confusingly
    !> (L shown as R or the reverse, or strong, weak and neutral
    !> characters at too high a level), or the end of the text.  Port of
    !> bidi.c:bidi_find_first_overridden.
    integer(int64) function bidi_find_first_overridden(it) result(found_pos)
        type(bidi_it_type), intent(inout) :: it
        integer :: max_l2r, max_r2l, max_weak, type, level, category
        integer(int64) :: eob

        eob = text_end(it)
        found_pos = eob
        max_r2l = 1

        if (it%paragraph_dir == L2R) then
            max_l2r = 0
            max_weak = 1
        else
            max_l2r = 2
            max_weak = 2
        end if

        do
            type = bidi_resolve_weak(it)
            level = it%level_stack(it%stack_idx)%level
            category = bidi_get_category(it%orig_type)
            ! Detect when we have overridden directionality, or have
            ! embedding levels larger than needed for the context.
            if ((type == STRONG_R .and. it%orig_type == STRONG_L) .or. &
                (type == STRONG_L .and. (it%orig_type == STRONG_R .or. it%orig_type == STRONG_AL)) .or. &
                ((it%orig_type == STRONG_L .or. it%orig_type == WEAK_EN) .and. level > max_l2r) .or. &
                ((it%orig_type == STRONG_R .or. it%orig_type == STRONG_AL) .and. level > max_r2l) .or. &
                ((category == WEAK .or. it%orig_type == NEUTRAL_ON) .and. level > max_weak)) found_pos = it%charpos
            if (found_pos /= eob .or. it%charpos >= eob .or. it%ch == BIDI_EOB .or. it%ch == 10) exit
        end do
    end function bidi_find_first_overridden
end module bidi_fm
