! bidirectional_mod.f90 -- the Unicode Bidirectional Algorithm (UBA).
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
!> (compute_display_string_position and compute_display_string_end of xdisp.c:
!> text a display string replaces counts as its characters), the
!> paragraph region cache (`cache_long_scans'), the buffer-local
!> `bidi_paragraph_start_regular_expression' and `bidi_paragraph_separate_regular_expression' (their
!> defaults are used), the redisplay ticks (`maximum_redisplay_ticks').
module bidirectional_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type
    use character_mod, only: MAXIMUM_CHARACTER, string_character_and_length, bytes_by_character_head
    use buffer_mod, only: buffers, current, buf_fetch_char, buf_fetch_byte, buf_next_char_len, buf_prev_char_len
    use marker_mod, only: character_to_byte, byte_to_character
    use search_mod, only: find_newline_no_quit, fast_looking_at
    use unicode_bidirectional_mod, only: bidi_runs, bidi_starts, bidi_ends, bidi_codes, mirror_count, mirror_from, mirror_to, &
                                         open_bracket_count, open_brackets, close_bracket_count, close_brackets
    implicit none(type, external)
    private

    public :: bidi_get_type, bidi_mirror_character, bidi_init_it, bidi_paragraph_init, bidi_move_to_visually_next, &
              bidi_find_first_overridden, bidi_level_start, bidi_push_it, bidi_pop_it, bidi_shelve_cache, &
              bidi_unshelve_cache, bidi_set_string, bidi_at_base_level

    !> The deepest embedding level (dispextern.h BIDI_MAXDEPTH).
    integer, parameter, public :: BIDI_MAXDEPTH = 125

    !> The bidirectional character types (dispextern.h bidi_type_t).
    integer, parameter, public :: UNKNOWN_BT = 0, STRONG_L = 1, STRONG_R = 2, WEAK_EN = 3, WEAK_AN = 4, &
                                  WEAK_BN = 5, NEUTRAL_B = 6, STRONG_AL = 7, LRE = 8, LRO = 9, RLE = 10, &
                                  RLO = 11, PDF = 12, LRI = 13, RLI = 14, FSI = 15, PDI = 16, WEAK_ES = 17, &
                                  WEAK_ET = 18, WEAK_CS = 19, WEAK_NSM = 20, NEUTRAL_S = 21, NEUTRAL_WHITESPACE = 22, &
                                  NEUTRAL_ON = 23

    !> The paired bracket types (dispextern.h bidi_bracket_type_t).
    integer, parameter, public :: BIDI_BRACKET_NONE = 1, BIDI_BRACKET_OPEN = 2, BIDI_BRACKET_CLOSE = 3

    !> The basic directions (dispextern.h bidi_directory_t).
    integer, parameter, public :: NEUTRAL_DIRECTORY = 0, L2R = 1, R2L = 2

    !> The end of the text, as a character (bidi.c BIDI_END_OF_BUFFER).
    integer(int32), parameter, public :: BIDI_END_OF_BUFFER = -1

    !> The categories of bidi types (bidi.c bidi_category_t).
    integer, parameter :: UNKNOWN_BC = 0, NEUTRAL = 1, WEAK = 2, STRONG = 3, EXPLICIT_FORMATTING = 4

    !> Inhibit the bidi bracket pair algorithm (BPA).  If true, the paired
    !> brackets are resolved as other neutral characters.  Port of xdisp.c
    !> bidi-inhibit-bpa.
    logical, public :: bidi_inhibit_bpa = .false.

    !> How many states are added to the cache at a time, and at most in
    !> one slot of the iterator stack (bidi.c).
    integer(int64), parameter :: BIDI_CACHE_CHUNK = 200, BIDI_CACHE_MAXIMUM_ELEMENTS_PER_SLOT = 50000
    !> dispextern.h IT_STACK_SIZE.
    integer, parameter :: IT_STACK_SIZE = 5

    !> What the iterator remembers of a character (dispextern.h struct
    !> bidi_saved_info).
    type, public :: bidi_saved_info_type
        integer(int64) :: character_position = 0
        integer :: type = UNKNOWN_BT, original_type = UNKNOWN_BT
    end type bidi_saved_info_type

    !> One level of the directional status stack (dispextern.h struct
    !> bidi_stack); `flags' holds the isolate status (bit 0), the override
    !> (bits 1-2) and the sos of an isolate (bit 3).
    type, public :: bidi_stack_type
        integer(int64) :: next_for_neutral_position = 0
        integer :: next_for_neutral_type = 0, last_strong_type = 0, previous_for_neutral_type = 0
        integer :: level = 0, flags = 0
    end type bidi_stack_type

    !> The string being reordered (dispextern.h struct bidi_string_data):
    !> none when not `active' (the current buffer is reordered).
    type, public :: bidi_string_type
        logical :: active = .false.
        character(len=:), allocatable :: s
        integer(int64) :: count_characters = 0
    end type bidi_string_type

    !> The bidi iterator (dispextern.h struct bidi_it).  The components
    !> from `level_stack' on (but its levels in use) are not part of the
    !> state that is saved, cached, pushed or popped.
    type, public :: bidi_it_type
        integer(int64) :: byte_position = 0, character_position = 0
        integer(int32) :: ch = 0
        integer(int64) :: nchars = 0, ch_length = 0
        integer :: type = UNKNOWN_BT, type_after_wn = UNKNOWN_BT, original_type = UNKNOWN_BT
        integer :: resolved_level = 0, isolate_level = 0
        integer(int64) :: invalid_levels = 0, invalid_isolates = 0
        type(bidi_saved_info_type) :: previous, last_strong, next_for_neutral, previous_for_neutral, next_for_whitespace
        integer(int64) :: bracket_pairing_position = 0
        integer :: bracket_enclosed_type = UNKNOWN_BT
        integer(int64) :: next_en_position = 0
        integer :: next_en_type = UNKNOWN_BT
        integer :: sos = L2R, scan_directory = 0
        integer(int64) :: disp_position = 0
        integer :: disp_property = 0
        integer :: stack_idx = 0
        type(bidi_stack_type) :: level_stack(0:BIDI_MAXDEPTH + 2)
        type(bidi_string_type) :: string
        integer :: paragraph_directory = NEUTRAL_DIRECTORY
        integer(int64) :: separator_limit = 0
        logical :: first_element = .false., new_paragraph = .false., frame_window_p = .false.
    end type bidi_it_type

    !> The cache's state, saved by `bidi_shelve_cache'.
    type, public :: bidi_shelved_type
        logical :: empty = .true.
        type(bidi_it_type), allocatable :: cache(:)
        integer(int64) :: index = 0, start = 0, last_idx = 0, maximum_elements = 0
        integer(int64) :: start_stack(0:IT_STACK_SIZE - 1) = 0
        integer :: sp = 0
    end type bidi_shelved_type

    !> The depth of the bracket stack of `bidi_find_bracket_pairs': as
    !> many entries as fit upstream in MAXIMUM_ALLOCA (16 KiB) less two
    !> iterators, on a 64-bit system (bidi.c MAXIMUM_BPA_STACK).
    integer, parameter :: MAXIMUM_BPA_STACK = 977

    !> The furthest find_first_strong_character looks, and paragraph start
    !> searches (bidi.c MAXIMUM_STRONG_CHARACTER_SEARCH, MAXIMUM_PARAGRAPH_SEARCH).
    integer(int64), parameter :: MAXIMUM_STRONG_CHARACTER_SEARCH = 100000, MAXIMUM_PARAGRAPH_SEARCH = 7500

    !> Bracket stack flags (bidi.c).
    integer, parameter :: FLAG_EMBEDDING_INSIDE = 1, FLAG_OPPOSITE_INSIDE = 2

    !> The cache of iterator states (bidi.c bidi_cache and its indices).
    type(bidi_it_type), allocatable :: bidi_cache(:)
    integer(int64) :: bidi_cache_size = 0, bidi_cache_idx = 0, bidi_cache_last_idx = -1, bidi_cache_start = 0
    integer(int64) :: bidi_cache_maximum_elements = BIDI_CACHE_MAXIMUM_ELEMENTS_PER_SLOT
    integer(int64) :: bidi_cache_start_stack(0:IT_STACK_SIZE - 1) = 0
    integer :: bidi_cache_sp = 0
    !> The characters find_first_strong_character examined (bidi.c).
    integer(int64) :: nsearch_for_strong = 0

contains

    ! ------------------------------------------------------------------
    ! Character properties
    ! ------------------------------------------------------------------

    !> The bidi class of character C (the bidi-class property table;
    !> L above U+10FFFF).  Not a port.
    integer function bidi_class(c) result(t)
        integer(int32), intent(in) :: c
        integer :: lo, hi, middle

        t = STRONG_L
        if (c > bidi_ends(bidi_runs)) return
        lo = 1
        hi = bidi_runs

        do while (lo < hi)
            middle = (lo + hi)/2

            if (bidi_ends(middle) < c) then
                lo = middle + 1
            else
                hi = middle
            end if
        end do

        t = bidi_codes(lo)
    end function bidi_class

    !> The index of C in the sorted array A, or 0.  Not a port.
    pure integer function search_sorted(a, c) result(k)
        integer(int32), intent(in) :: a(:), c
        integer :: lo, hi, middle

        k = 0
        lo = 1
        hi = size(a)

        do while (lo <= hi)
            middle = (lo + hi)/2

            if (a(middle) == c) then
                k = middle
                return
            else if (a(middle) < c) then
                lo = middle + 1
            else
                hi = middle - 1
            end if
        end do
    end function search_sorted

    !> The bidi type of character CH, with OVERRIDE (L2R or R2L) making
    !> it strong unless it is a formatting character.  Port of
    !> bidi.c:bidi_get_type.
    integer function bidi_get_type(ch, override) result(t)
        integer(int32), intent(in) :: ch
        integer, intent(in) :: override

        if (ch == BIDI_END_OF_BUFFER) then
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
        case (NEUTRAL_B, NEUTRAL_S, NEUTRAL_WHITESPACE, NEUTRAL_ON)
            c = NEUTRAL
        case (LRE, LRO, RLE, RLO, PDF, LRI, RLI, FSI, PDI)
            c = EXPLICIT_FORMATTING
        case default
            c = UNKNOWN_BC
        end select
    end function bidi_get_category

    !> Whether TYPE is an isolate formatting character.  Port of
    !> bidi.c:bidi_isolate_fmt_char.
    pure logical function bidi_isolate_fmt_character(type)
        integer, intent(in) :: type

        bidi_isolate_fmt_character = type == LRI .or. type == RLI .or. type == PDI .or. type == FSI
    end function bidi_isolate_fmt_character

    !> The mirror image of character C (C itself if it has none).  Port of
    !> bidi.c:bidi_mirror_char.
    integer(int32) function bidi_mirror_character(c) result(m)
        integer(int32), intent(in) :: c
        integer :: k

        m = c
        if (c == BIDI_END_OF_BUFFER) return
        k = search_sorted(mirror_from, c)
        if (k > 0) m = mirror_to(k)
    end function bidi_mirror_character

    !> The paired bracket type of character C.  Port of
    !> bidi.c:bidi_paired_bracket_type.
    integer function bidi_paired_bracket_type(c) result(t)
        integer(int32), intent(in) :: c

        t = BIDI_BRACKET_NONE
        if (c == BIDI_END_OF_BUFFER .or. bidi_inhibit_bpa) return

        if (search_sorted(open_brackets, c) > 0) then
            t = BIDI_BRACKET_OPEN
        else if (search_sorted(close_brackets, c) > 0) then
            t = BIDI_BRACKET_CLOSE
        end if
    end function bidi_paired_bracket_type

    !> Whether CH is an explicit embedding or override character (LRE,
    !> LRO, RLE, RLO or PDF).  Port of bidi.c:bidi_explicit_dir_char.
    logical function bidi_explicit_directory_character(ch)
        integer(int32), intent(in) :: ch
        integer :: t

        bidi_explicit_directory_character = .false.
        if (ch < 0) return
        t = bidi_class(ch)
        bidi_explicit_directory_character = t == LRE .or. t == LRO .or. t == RLE .or. t == RLO .or. t == PDF
    end function bidi_explicit_directory_character

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

        it%previous%type = UNKNOWN_BT
        it%last_strong%type = UNKNOWN_BT
        it%last_strong%original_type = UNKNOWN_BT

        if (it%sos == R2L) then
            it%previous_for_neutral%type = STRONG_R
        else
            it%previous_for_neutral%type = STRONG_L
        end if

        it%previous_for_neutral%character_position = it%character_position
        it%next_for_neutral%type = UNKNOWN_BT
        it%next_for_neutral%original_type = UNKNOWN_BT
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
        integer :: previous_level

        previous_level = it%level_stack(it%stack_idx)%level
        it%stack_idx = it%stack_idx + 1

        associate (st => it%level_stack(it%stack_idx))
            st%level = level
            st%flags = ishft(iand(override, 3), 1)

            if (isolate) then
                st%flags = ior(st%flags, 1)
                st%last_strong_type = it%last_strong%type
                st%previous_for_neutral_type = it%previous_for_neutral%type
                st%next_for_neutral_type = it%next_for_neutral%type
                st%next_for_neutral_position = it%next_for_neutral%character_position
                if (it%sos /= L2R) st%flags = ior(st%flags, 8)
            end if
        end associate

        call bidi_set_sos_type(it, previous_level, level)
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
                it%previous%type = UNKNOWN_BT
                it%previous%original_type = UNKNOWN_BT
                it%last_strong%type = st%last_strong_type
                it%previous_for_neutral%type = st%previous_for_neutral_type
                it%next_for_neutral%type = st%next_for_neutral_type
                it%next_for_neutral%character_position = st%next_for_neutral_position

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
    pure subroutine bidi_remember_character(saved, it, from_type)
        type(bidi_saved_info_type), intent(out) :: saved
        type(bidi_it_type), intent(in) :: it
        logical, intent(in) :: from_type

        saved%character_position = it%character_position

        if (from_type) then
            saved%type = it%type
        else
            saved%type = it%type_after_wn
        end if

        saved%original_type = it%original_type
    end subroutine bidi_remember_character

    !> Copy the state of iterator FROM to TO: everything before the
    !> stack, and the stack up to its top.  Port of bidi.c:bidi_copy_it.
    pure subroutine bidi_copy_it(to, from)
        type(bidi_it_type), intent(inout) :: to
        type(bidi_it_type), intent(in) :: from

        to%byte_position = from%byte_position
        to%character_position = from%character_position
        to%ch = from%ch
        to%nchars = from%nchars
        to%ch_length = from%ch_length
        to%type = from%type
        to%type_after_wn = from%type_after_wn
        to%original_type = from%original_type
        to%resolved_level = from%resolved_level
        to%isolate_level = from%isolate_level
        to%invalid_levels = from%invalid_levels
        to%invalid_isolates = from%invalid_isolates
        to%previous = from%previous
        to%last_strong = from%last_strong
        to%next_for_neutral = from%next_for_neutral
        to%previous_for_neutral = from%previous_for_neutral
        to%next_for_whitespace = from%next_for_whitespace
        to%bracket_pairing_position = from%bracket_pairing_position
        to%bracket_enclosed_type = from%bracket_enclosed_type
        to%next_en_position = from%next_en_position
        to%next_en_type = from%next_en_type
        to%sos = from%sos
        to%scan_directory = from%scan_directory
        to%disp_position = from%disp_position
        to%disp_property = from%disp_property
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
        bidi_cache_maximum_elements = BIDI_CACHE_MAXIMUM_ELEMENTS_PER_SLOT
    end subroutine bidi_cache_shrink

    !> Load the cached state INDEX into the iterator, keeping its scan
    !> direction.  Port of bidi.c:bidi_cache_fetch_state.
    subroutine bidi_cache_fetch_state(index, it)
        integer(int64), intent(in) :: index
        type(bidi_it_type), intent(inout) :: it
        integer :: current_scan_directory

        current_scan_directory = it%scan_directory
        call bidi_copy_it(it, bidi_cache(index))
        it%scan_directory = current_scan_directory
        bidi_cache_last_idx = index
    end subroutine bidi_cache_fetch_state

    !> The index of the cached state that covers CHARACTER_POSITION at level LEVEL
    !> or lower (any level if -1), searching in direction DIRECTORY from the
    !> last state used, or -1.  Port of bidi.c:bidi_cache_search.
    integer(int64) function bidi_cache_search(character_position, level, directory_in) result(found)
        integer(int64), intent(in) :: character_position
        integer, intent(in) :: level, directory_in
        integer(int64) :: i, i_start
        integer :: directory

        found = -1
        directory = directory_in
        if (bidi_cache_idx <= bidi_cache_start) return
        if (bidi_cache_last_idx == -1) bidi_cache_last_idx = bidi_cache_idx - 1

        if (character_position < bidi_cache(bidi_cache_last_idx)%character_position) then
            directory = -1
            i_start = bidi_cache_last_idx - 1
        else if (character_position > bidi_cache(bidi_cache_last_idx)%character_position + bidi_cache(bidi_cache_last_idx)%nchars &
                 - 1) then
            directory = 1
            i_start = bidi_cache_last_idx + 1
        else if (directory /= 0) then
            i_start = bidi_cache_last_idx
        else
            directory = -1
            i_start = bidi_cache_idx - 1
        end if

        if (directory < 0) then
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

        !> Whether state I covers CHARACTER_POSITION at a level LEVEL allows.
        logical function covers(i)
            integer(int64), intent(in) :: i

            covers = .false.
            if (bidi_cache(i)%character_position > character_position) return
            if (character_position >= bidi_cache(i)%character_position + bidi_cache(i)%nchars) return
            covers = level == -1 .or. bidi_cache(i)%resolved_level <= level
        end function covers
    end function bidi_cache_search

    !> The index of the cached state where the level drops below LEVEL,
    !> searching from the last state used in direction DIRECTORY (from the end
    !> if 0); with BEFORE, the state before that change.  -1 if none.
    !> Port of bidi.c:bidi_cache_find_level_change.
    integer(int64) function bidi_cache_find_level_change(level, directory_in, before) result(found)
        integer, intent(in) :: level, directory_in
        logical, intent(in) :: before
        integer(int64) :: i
        integer :: increment, directory

        found = -1
        if (bidi_cache_idx == 0) return
        directory = directory_in

        if (directory /= 0) then
            i = bidi_cache_last_idx
        else
            i = bidi_cache_idx - 1
        end if

        increment = 0
        if (before) increment = 1
        if (i < 0) i = 0

        if (directory == 0) then
            directory = -1
        else if (increment == 0) then
            i = i + directory
        end if

        if (directory < 0) then
            do while (i >= bidi_cache_start + increment)
                if (bidi_cache(i - increment)%resolved_level >= 0 .and. bidi_cache(i - increment)%resolved_level < level) then
                    found = i
                    return
                end if

                i = i - 1
            end do
        else
            do while (i < bidi_cache_idx - increment)
                if (bidi_cache(i + increment)%resolved_level >= 0 .and. bidi_cache(i + increment)%resolved_level < level) then
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
    integer(int64) function bidi_level_start(level) result(position)
        integer, intent(in) :: level
        integer(int64) :: slot

        position = -1
        slot = bidi_cache_find_level_change(level, -1, .true.)
        if (slot >= 0) position = bidi_cache(slot)%character_position
    end function bidi_level_start

    !> Make room in the cache for the state at index INDEX, if the slot
    !> allows that many.  Port of bidi.c:bidi_cache_ensure_space.
    subroutine bidi_cache_ensure_space(index)
        integer(int64), intent(in) :: index
        type(bidi_it_type), allocatable :: grown(:)
        integer(int64) :: chunk_size, new_size

        if (index < bidi_cache_size) return
        chunk_size = BIDI_CACHE_CHUNK
        if (bidi_cache_size > bidi_cache_maximum_elements - chunk_size) chunk_size = bidi_cache_maximum_elements - bidi_cache_size
        if (max(index + 1, bidi_cache_size + chunk_size) > bidi_cache_maximum_elements) return
        new_size = min(bidi_cache_maximum_elements, bidi_cache_size + max(chunk_size, index - bidi_cache_size + 1))
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
        index = bidi_cache_search(it%character_position, -1, 1)
        if (index < 0 .and. update_only) return

        if (index < 0) then
            index = bidi_cache_idx
            call bidi_cache_ensure_space(index)

            ! Don't let the cache grow out of contiguity.
            if (bidi_cache_start < index .and. index < bidi_cache_size) then
                if (it%character_position > bidi_cache(index - 1)%character_position + bidi_cache(index - 1)%nchars .or. &
                    it%character_position < bidi_cache(bidi_cache_start)%character_position) then
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
            bidi_cache(index)%next_for_whitespace = it%next_for_whitespace
            bidi_cache(index)%disp_position = it%disp_position
            bidi_cache(index)%disp_property = it%disp_property
            bidi_cache(index)%bracket_pairing_position = it%bracket_pairing_position
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

    !> Load the cached state at CHARACTER_POSITION (only a resolved one if
    !> RESOLVED_ONLY) into the iterator and return its type, or
    !> UNKNOWN_BT.  Port of bidi.c:bidi_cache_find.
    integer function bidi_cache_find(character_position, resolved_only, it) result(type)
        integer(int64), intent(in) :: character_position
        logical, intent(in) :: resolved_only
        type(bidi_it_type), intent(inout) :: it
        integer(int64) :: i
        integer :: current_scan_directory

        type = UNKNOWN_BT
        i = bidi_cache_search(character_position, -1, it%scan_directory)
        if (i < bidi_cache_start) return
        if (resolved_only .and. bidi_cache(i)%resolved_level < 0) return
        current_scan_directory = it%scan_directory
        call bidi_copy_it(it, bidi_cache(i))
        bidi_cache_last_idx = i
        it%scan_directory = current_scan_directory
        type = it%type
    end function bidi_cache_find

    !> The level of the next character in the scan direction, from the
    !> cache.  Port of bidi.c:bidi_peek_at_next_level.
    integer function bidi_peek_at_next_level(it) result(level)
        type(bidi_it_type), intent(in) :: it

        if (bidi_cache_last_idx == -1 .or. (bidi_cache_last_idx >= bidi_cache_idx - 1 .and. it%scan_directory > 0)) then
            level = bidi_cache(bidi_cache_idx - 1)%resolved_level
        else
            level = bidi_cache(bidi_cache_last_idx + it%scan_directory)%resolved_level
        end if
    end function bidi_peek_at_next_level

    !> Push the iterator's state on the cache and start a new slot (for
    !> reordering a string within the buffer text).  Port of
    !> bidi.c:bidi_push_it.
    subroutine bidi_push_it(it)
        type(bidi_it_type), intent(in) :: it

        bidi_cache_maximum_elements = bidi_cache_maximum_elements + BIDI_CACHE_MAXIMUM_ELEMENTS_PER_SLOT
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
        bidi_cache_maximum_elements = bidi_cache_maximum_elements - BIDI_CACHE_MAXIMUM_ELEMENTS_PER_SLOT
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
        data%maximum_elements = bidi_cache_maximum_elements
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
                bidi_cache_maximum_elements = BIDI_CACHE_MAXIMUM_ELEMENTS_PER_SLOT
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
            bidi_cache_maximum_elements = data%maximum_elements
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
        it%string%count_characters = 0
        if (allocated(it%string%s)) deallocate (it%string%s)
        if (.not. present(s)) return
        it%string%s = s
        k = 1

        do while (k <= len(s, kind=int64))
            call string_character_and_length(s, k, ch, n)
            k = k + n
            it%string%count_characters = it%string%count_characters + 1
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

    !> Set up IT to reorder from CHARACTER_POSITION and BYTE_POSITION (unchanged where
    !> negative).  Port of bidi.c:bidi_init_it (with
    !> bidi.c:bidi_initialize: the tables are compiled in).
    subroutine bidi_init_it(character_position, byte_position, frame_window_p, it)
        integer(int64), intent(in) :: character_position, byte_position
        logical, intent(in) :: frame_window_p
        type(bidi_it_type), intent(inout) :: it

        if (character_position >= 0) it%character_position = character_position
        if (byte_position >= 0) it%byte_position = byte_position
        it%frame_window_p = frame_window_p
        ! This means "need to recompute".
        it%nchars = -1
        it%first_element = .true.
        call bidi_set_paragraph_end(it)
        it%new_paragraph = .true.
        it%separator_limit = -1
        it%type = NEUTRAL_B
        it%type_after_wn = NEUTRAL_B
        it%original_type = NEUTRAL_B
        it%previous%type = UNKNOWN_BT
        it%previous%original_type = UNKNOWN_BT
        it%last_strong%type = UNKNOWN_BT
        it%last_strong%original_type = UNKNOWN_BT
        it%next_for_neutral%character_position = -1
        it%next_for_neutral%type = UNKNOWN_BT
        it%next_for_neutral%original_type = UNKNOWN_BT
        it%previous_for_neutral%character_position = -1
        it%previous_for_neutral%type = UNKNOWN_BT
        it%previous_for_neutral%original_type = UNKNOWN_BT
        it%bracket_pairing_position = -1
        it%sos = L2R
        it%disp_position = -1
        it%disp_property = 0

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

        it%scan_directory = 1
        it%stack_idx = 0
        it%resolved_level = it%level_stack(0)%level
        it%level_stack(0)%flags = 0
        it%invalid_levels = 0
        it%isolate_level = 0
        it%invalid_isolates = 0
        it%next_en_position = 0
        it%next_en_type = UNKNOWN_BT
        it%next_for_whitespace%character_position = -1
        it%next_for_whitespace%type = UNKNOWN_BT
        it%bracket_pairing_position = -1
        before = 0
        if (it%paragraph_directory == R2L) before = 1
        call bidi_set_sos_type(it, before, it%level_stack(0)%level)
        call bidi_cache_reset()
    end subroutine bidi_line_init

    ! ------------------------------------------------------------------
    ! Fetching characters
    ! ------------------------------------------------------------------

    !> The end of the text: the string's length, or END_OF_ACCESSIBLE_PORTION.  Not a port.
    integer(int64) function text_end(it) result(e)
        type(bidi_it_type), intent(in) :: it

        if (it%string%active) then
            e = it%string%count_characters
        else
            e = buffers(current)%end_of_accessible_portion
        end if
    end function text_end

    !> The character at BYTE_POSITION of the text and its length in bytes.
    !> Port of bidi.c:bidi_char_at_pos (with bidi.c:bidi_count_bytes for
    !> the length).
    subroutine bidi_character_at_position(it, byte_position, ch, len)
        type(bidi_it_type), intent(in) :: it
        integer(int64), intent(in) :: byte_position
        integer(int32), intent(out) :: ch
        integer(int64), intent(out) :: len
        integer(int32) :: n

        if (it%string%active) then
            call string_character_and_length(it%string%s, byte_position + 1, ch, n)
            len = n
        else
            ch = buf_fetch_char(buffers(current), byte_position)
            len = buf_next_char_len(buffers(current), byte_position)
        end if
    end subroutine bidi_character_at_position

    !> The character at CHARACTER_POSITION and BYTE_POSITION, BIDI_END_OF_BUFFER at the end, with
    !> its length in bytes and characters.  Port of bidi.c:bidi_fetch_char
    !> (display properties are Deferred: DISP_POSITION is the end of the text).
    integer(int32) function bidi_fetch_character(it, character_position, byte_position, disp_position, disp_property, ch_length, &
                                                 nchars) result(ch)
        type(bidi_it_type), intent(in) :: it
        integer(int64), intent(in) :: character_position, byte_position
        integer(int64), intent(inout) :: disp_position
        integer, intent(inout) :: disp_property
        integer(int64), intent(out) :: ch_length, nchars
        integer(int64) :: endpos

        endpos = text_end(it)
        disp_position = endpos
        disp_property = 0
        nchars = 1

        if (character_position >= endpos) then
            ch = BIDI_END_OF_BUFFER
            ch_length = 1
        else
            call bidi_character_at_position(it, byte_position, ch, ch_length)
        end if
    end function bidi_fetch_character

    !> Like `bidi_fetch_character', but an isolate initiator comes with the
    !> isolated text up to its matching PDI, as one character.  Port of
    !> bidi.c:bidi_fetch_char_skip_isolates.
    integer(int32) function bidi_fetch_character_skip_isolates(it, character_position_in, byte_position_in, disp_position, &
                                                               disp_property, ch_length, &
                                                               nchars) result(ch)
        type(bidi_it_type), intent(in) :: it
        integer(int64), intent(in) :: character_position_in, byte_position_in
        integer(int64), intent(inout) :: disp_position
        integer, intent(inout) :: disp_property
        integer(int64), intent(out) :: ch_length, nchars
        integer(int64) :: character_position, byte_position, level
        integer :: ch_type

        character_position = character_position_in
        byte_position = byte_position_in
        ch = bidi_fetch_character(it, character_position, byte_position, disp_position, disp_property, ch_length, nchars)
        ch_type = bidi_get_type(ch, NEUTRAL_DIRECTORY)
        level = 0

        if (ch_type == LRI .or. ch_type == RLI .or. ch_type == FSI) then
            level = level + 1

            do while (level > 0 .and. ch_type /= NEUTRAL_B)
                character_position = character_position + nchars
                byte_position = byte_position + ch_length
                ch = bidi_fetch_character(it, character_position, byte_position, disp_position, disp_property, ch_length, nchars)
                ch_type = bidi_get_type(ch, NEUTRAL_DIRECTORY)

                if (ch_type == LRI .or. ch_type == RLI .or. ch_type == FSI) then
                    level = level + 1
                else if (ch_type == PDI) then
                    level = level - 1
                end if
            end do
        end if

        ! Make sure the data for the isolate is all of it.
        nchars = nchars + character_position - character_position_in
        ch_length = ch_length + byte_position - byte_position_in
    end function bidi_fetch_character_skip_isolates

    ! ------------------------------------------------------------------
    ! Paragraphs
    ! ------------------------------------------------------------------

    !> The regexps of a paragraph separator and start (bidi.c
    !> paragraph_separate_regular_expression, paragraph_start_regular_expression).  Not a port.
    function paragraph_regular_expression(start) result(regular_expression)
        logical, intent(in) :: start
        character(len=:), allocatable :: regular_expression

        if (start) then
            regular_expression = "^\("//achar(12)//"\|[ "//achar(9)//"]*\)$"
        else
            regular_expression = "^[ "//achar(9)//achar(12)//"]*$"
        end if
    end function paragraph_regular_expression

    !> The length of the paragraph separator at CHARACTER_POSITION (0 for an empty
    !> line), -1 if a paragraph starts there without a separator, -2 if
    !> neither.  Port of bidi.c:bidi_at_paragraph_end.
    integer(int64) function bidi_at_paragraph_end(character_position) result(value)
        integer(int64), intent(in) :: character_position
        type(status_type) :: st

        value = fast_looking_at(paragraph_regular_expression(.false.), character_position, &
                                buffers(current)%end_of_accessible_portion, st)

        if (value < 0) then
            if (fast_looking_at(paragraph_regular_expression(.true.), character_position, &
                                buffers(current)%end_of_accessible_portion, st) >= 0) then
                value = -1
            else
                value = -2
            end if
        end if
    end function bidi_at_paragraph_end

    !> The byte position of the start of the paragraph holding POSITION (the
    !> line where a paragraph start matches, at most MAXIMUM_PARAGRAPH_SEARCH
    !> lines back, else BEGINNING_OF_ACCESSIBLE_PORTION).  Port of bidi.c:bidi_find_paragraph_start
    !> (without the region cache of bidi_paragraph_cache_on_off, Deferred).
    integer(int64) function bidi_find_paragraph_start(position_in, position_byte_in) result(position_byte)
        integer(int64), intent(in) :: position_in, position_byte_in
        integer(int64) :: position, n
        type(status_type) :: st

        position = position_in
        position_byte = position_byte_in
        n = 0

        do while (position_byte > buffers(current)%beginning_of_accessible_portion_byte)
            n = n + 1
            if (n > MAXIMUM_PARAGRAPH_SEARCH) exit
            if (fast_looking_at(paragraph_regular_expression(.true.), position, buffers(current)%end_of_accessible_portion, &
                                st) >= 0) exit
            ! dec_both
            position = position - 1
            position_byte = position_byte - buf_prev_char_len(buffers(current), position_byte)
            position = find_newline_no_quit(position, position_byte, -1_int64, position_byte)
        end do

        if (n >= MAXIMUM_PARAGRAPH_SEARCH) position_byte = buffers(current)%beginning_of_accessible_portion_byte
        position_byte = max(buffers(current)%beginning_of_accessible_portion_byte, min(position_byte, &
                                                                                   buffers(current)%end_of_accessible_portion_byte))
    end function bidi_find_paragraph_start

    !> The type of the first strong character from POSITION (or NEUTRAL_B at a
    !> paragraph's end, or PDI with STOP_AT_PDI), isolates skipped.
    !> Port of bidi.c:find_first_strong_char.
    integer function find_first_strong_character(it, position_in, byte_position_in, end, disp_position, disp_property, ch_length, &
                                                 nchars, &
                                                 stop_at_pdi) result(type)
        type(bidi_it_type), intent(in) :: it
        integer(int64), intent(in) :: position_in, byte_position_in, end
        integer(int64), intent(inout) :: disp_position
        integer, intent(inout) :: disp_property
        integer(int64), intent(inout) :: ch_length, nchars
        logical, intent(in) :: stop_at_pdi
        integer(int64) :: position, byte_position, position1
        integer(int32) :: ch

        position = position_in
        byte_position = byte_position_in

        if (stop_at_pdi) then
            ! Skip the FSI itself.
            position = position + nchars
            byte_position = byte_position + ch_length
        end if

        ch = bidi_fetch_character_skip_isolates(it, position, byte_position, disp_position, disp_property, ch_length, nchars)
        type = bidi_get_type(ch, NEUTRAL_DIRECTORY)
        position1 = position
        position = position + nchars
        byte_position = byte_position + ch_length

        do while (bidi_get_category(type) /= STRONG .and. .not. (stop_at_pdi .and. type == PDI) .and. &
                  position - position1 < MAXIMUM_STRONG_CHARACTER_SEARCH)
            if (position >= end) then
                ! Pretend there's a paragraph separator at end of buffer.
                type = NEUTRAL_B
                exit
            end if

            if (.not. it%string%active .and. type == NEUTRAL_B) then
                if (bidi_at_paragraph_end(position) >= -1) exit
            end if

            ch = bidi_fetch_character_skip_isolates(it, position, byte_position, disp_position, disp_property, ch_length, nchars)
            position = position + nchars
            byte_position = byte_position + ch_length
            type = bidi_get_type(ch, NEUTRAL_DIRECTORY)
        end do

        nsearch_for_strong = nsearch_for_strong + position - position1
    end function find_first_strong_character

    !> Determine the base direction of the paragraph at the iterator: DIRECTORY
    !> if L2R or R2L, else from its first strong character (with
    !> NO_DEFAULT_P, that of an earlier paragraph if it has none), and
    !> start a line.  Port of bidi.c:bidi_paragraph_init.
    subroutine bidi_paragraph_init(directory_in, it, no_default_p)
        integer, intent(in) :: directory_in
        type(bidi_it_type), intent(inout) :: it
        logical, intent(in) :: no_default_p
        integer(int64) :: byte_position, pstartbyte, begbyte, end, position, ch_length, nchars, disp_position, prevpbyte, p, pbyte
        integer(int64) :: len
        integer(int32) :: ch
        integer :: directory, type, disp_property

        byte_position = it%byte_position
        begbyte = buffers(current)%beginning_of_accessible_portion_byte
        end = buffers(current)%end_of_accessible_portion

        if (it%string%active) then
            begbyte = 0
            end = it%string%count_characters
        end if

        position = it%character_position
        nsearch_for_strong = 0
        directory = directory_in
        ! Special case for an empty buffer.
        if (byte_position == begbyte .and. it%character_position == end) directory = L2R

        if (directory == L2R) then
            it%paragraph_directory = L2R
            it%new_paragraph = .false.
        else if (directory == R2L) then
            it%paragraph_directory = R2L
            it%new_paragraph = .false.
        else
            ! Use the default heuristics: the first strong character.
            disp_position = -1
            disp_property = 0
            if (.not. it%first_element .and. it%character_position < it%separator_limit) return
            position = it%character_position

            if (byte_position > begbyte) then
                call bidi_character_at_position(it, byte_position, ch, len)

                if (ch == 10) then
                    byte_position = byte_position + 1
                    position = position + 1
                end if
            end if

            if (it%string%active) then
                pstartbyte = 0
            else
                pstartbyte = bidi_find_paragraph_start(position, byte_position)
            end if

            it%separator_limit = -1
            it%new_paragraph = .false.

            do
                byte_position = pstartbyte
                if (.not. it%string%active) position = byte_to_character(byte_position)
                ch_length = 0
                nchars = 0
                type = find_first_strong_character(it, position, byte_position, end, disp_position, disp_property, ch_length, &
                                                   nchars, .false.)

                if (type == STRONG_R .or. type == STRONG_AL) then
                    it%paragraph_directory = R2L
                else if (type == STRONG_L) then
                    it%paragraph_directory = L2R
                end if

                if (.not. it%string%active .and. no_default_p .and. it%paragraph_directory == NEUTRAL_DIRECTORY) then
                    ! Search back for a paragraph with a strong character.
                    if (pstartbyte == buffers(current)%beginning_of_accessible_portion_byte) then
                        it%paragraph_directory = L2R
                    else
                        prevpbyte = pstartbyte
                        pbyte = pstartbyte
                        p = byte_to_character(pstartbyte)

                        do while (pbyte > buffers(current)%beginning_of_accessible_portion_byte .and. prevpbyte >= pstartbyte)
                            p = p - 1
                            pbyte = pbyte - buf_prev_char_len(buffers(current), pbyte)
                            prevpbyte = bidi_find_paragraph_start(p, pbyte)
                        end do

                        pstartbyte = prevpbyte
                    end if
                end if

                if (it%string%active .or. .not. no_default_p .or. it%paragraph_directory /= NEUTRAL_DIRECTORY) exit
            end do
        end if

        ! Contrary to UAX#9 clause P3, we only default the paragraph
        ! direction to L2R if we have no previous usable paragraph.
        if (it%paragraph_directory /= L2R .and. it%paragraph_directory /= R2L) it%paragraph_directory = L2R

        if (it%paragraph_directory == R2L) then
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
        integer :: type, typ1, previous_type, current_level, new_level, override, disp_property
        integer :: stack_override
        logical :: isolate
        integer(int64) :: ch_length, nchars, disp_position, end, end_of_buffer, begbyte

        previous_type = UNKNOWN_BT
        end_of_buffer = text_end(it)

        if (it%type_after_wn /= WEAK_BN .and. it%type /= WEAK_BN) then
            ! A closing bracket resolved to a strong type by the BPA is
            ! remembered with that type (N0).
            if (it%type_after_wn == NEUTRAL_ON .and. bidi_get_category(it%type) == STRONG .and. &
                bidi_paired_bracket_type(it%ch) == BIDI_BRACKET_CLOSE) then
                call bidi_remember_character(it%previous, it, .true.)
            else
                call bidi_remember_character(it%previous, it, .false.)
            end if
        end if

        if (it%type_after_wn == STRONG_R .or. it%type_after_wn == STRONG_L .or. it%type_after_wn == STRONG_AL) then
            call bidi_remember_character(it%last_strong, it, .false.)
        end if

        if (it%type == STRONG_R .or. it%type == STRONG_L .or. it%type == WEAK_EN .or. it%type == WEAK_AN) then
            call bidi_remember_character(it%previous_for_neutral, it, .true.)
        end if

        ! If we overstepped the characters used for resolving neutrals and
        ! whitespace, invalidate their info.
        if (it%character_position >= it%next_for_neutral%character_position) then
            it%next_for_neutral%type = UNKNOWN_BT
            if (it%bracket_pairing_position == end_of_buffer) it%bracket_pairing_position = -1
        end if

        if (it%next_en_position >= 0 .and. it%character_position >= it%next_en_position) then
            it%next_en_position = 0
            it%next_en_type = UNKNOWN_BT
        end if

        ! Reset the bracket resolution info, unless the BPA was found
        ! unnecessary for this isolating run sequence.
        if (it%bracket_pairing_position /= end_of_buffer) then
            it%bracket_pairing_position = -1
            it%bracket_enclosed_type = UNKNOWN_BT
        end if

        begbyte = buffers(current)%beginning_of_accessible_portion_byte
        if (it%string%active) begbyte = 0

        if (it%byte_position < begbyte .or. it%first_element) then
            it%first_element = .false.

            if (it%string%active) then
                if (it%character_position < 0) then
                    it%character_position = 0
                    it%byte_position = 0
                end if
            else if (it%character_position < buffers(current)%beginning_of_accessible_portion) then
                it%character_position = buffers(current)%beginning_of_accessible_portion
                it%byte_position = buffers(current)%beginning_of_accessible_portion_byte
            end if

            ! The previous character, if any, is a newline (bidi_line_init).
            if (it%first_element .and. it%previous%type /= UNKNOWN_BT) previous_type = it%previous%original_type
        else if (it%character_position < end_of_buffer) then
            it%character_position = it%character_position + it%nchars
            it%byte_position = it%byte_position + it%ch_length
            previous_type = it%original_type
        else
            ! At the end of the text, the previous type is NEUTRAL_B.
            previous_type = NEUTRAL_B
        end if

        current_level = it%level_stack(it%stack_idx)%level
        isolate = isolate_status(it, it%stack_idx)
        override = override_of(it, it%stack_idx)
        new_level = current_level

        if (it%character_position >= end_of_buffer) then
            curchar = BIDI_END_OF_BUFFER
            it%ch_length = 1
            it%nchars = 1
            it%disp_position = end_of_buffer
            it%disp_property = 0
        else
            ! The level of an isolate initiator or a PDF changes after it.
            select case (previous_type)
            case (RLI)
                if (current_level < BIDI_MAXDEPTH .and. it%invalid_levels == 0 .and. it%invalid_isolates == 0) then
                    new_level = iand(current_level + 1, not(1)) + 1
                    it%isolate_level = it%isolate_level + 1
                    call bidi_push_embedding_level(it, new_level, NEUTRAL_DIRECTORY, .true.)
                else
                    it%invalid_isolates = it%invalid_isolates + 1
                end if
            case (LRI)
                if (current_level < BIDI_MAXDEPTH - 1 .and. it%invalid_levels == 0 .and. it%invalid_isolates == 0) then
                    new_level = iand(current_level + 2, not(1))
                    it%isolate_level = it%isolate_level + 1
                    call bidi_push_embedding_level(it, new_level, NEUTRAL_DIRECTORY, .true.)
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

            curchar = bidi_fetch_character(it, it%character_position, it%byte_position, it%disp_position, it%disp_property, &
                                           it%ch_length, it%nchars)
        end if

        it%ch = curchar
        it%resolved_level = new_level
        ! Don't apply directional override here, as all the types we handle
        ! below will not be affected by the override anyway.
        type = bidi_get_type(curchar, NEUTRAL_DIRECTORY)
        it%original_type = type
        it%type_after_wn = UNKNOWN_BT

        select case (type)
        case (RLE, RLO)
            it%type_after_wn = type
            ! X9/Retaining
            type = WEAK_BN

            if (new_level < BIDI_MAXDEPTH .and. it%invalid_levels == 0 .and. it%invalid_isolates == 0) then
                new_level = iand(new_level + 1, not(1)) + 1

                if (it%type_after_wn == RLE) then
                    override = NEUTRAL_DIRECTORY
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
                    override = NEUTRAL_DIRECTORY
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
                end = end_of_buffer
                disp_position = it%disp_position
                disp_property = it%disp_property
                nchars = it%nchars
                ch_length = it%ch_length
                typ1 = find_first_strong_character(it, it%character_position, it%byte_position, end, disp_position, disp_property, &
                                                   ch_length, nchars, &
                                                   .true.)

                if (typ1 /= STRONG_R .and. typ1 /= STRONG_AL) then
                    type = LRI
                else
                    type = RLI
                end if

                it%original_type = type
            end if

            if (override == NEUTRAL_DIRECTORY) then
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
        integer :: override, previous_level, new_level, type_of_next
        integer(int32) :: next_character
        integer(int64) :: end_of_buffer, en_position, len
        type(bidi_it_type) :: saved_it

        previous_level = it%level_stack(it%stack_idx)%level
        new_level = bidi_resolve_explicit(it)
        end_of_buffer = text_end(it)
        type = it%type
        override = override_of(it, it%stack_idx)
        ! Reset sos at a paragraph separator.
        if (it%type == NEUTRAL_B) call bidi_set_sos_type(it, previous_level, new_level)

        if (type == NEUTRAL_S .or. type == NEUTRAL_WHITESPACE .or. type == WEAK_BN .or. type == STRONG_AL) then
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
                if (it%previous%type /= UNKNOWN_BT .and. it%previous%type /= NEUTRAL_B) then
                    if (bidi_isolate_fmt_character(it%previous%type)) then
                        type = NEUTRAL_ON
                    else
                        type = it%previous%type
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
            else if ((type == WEAK_ES .and. it%previous%type == WEAK_EN .and. it%previous%original_type == WEAK_EN) .or. &
                     (type == WEAK_CS .and. ((it%previous%type == WEAK_EN .and. it%previous%original_type == WEAK_EN) .or. &
                                             it%previous%type == WEAK_AN))) then
                ! W4
                if (it%character_position + it%nchars >= end_of_buffer) then
                    next_character = BIDI_END_OF_BUFFER
                else
                    call bidi_character_at_position(it, it%byte_position + it%ch_length, next_character, len)
                end if

                type_of_next = bidi_get_type(next_character, override)

                if (type_of_next == WEAK_BN .or. bidi_explicit_directory_character(next_character)) then
                    call bidi_copy_it(saved_it, it)

                    do while (bidi_resolve_explicit(it) == new_level .and. it%type == WEAK_BN)
                        type_of_next = it%type
                    end do

                    call bidi_copy_it(it, saved_it)
                end if

                if (type == WEAK_ES .and. type_of_next == WEAK_EN .and. it%last_strong%type /= STRONG_AL) then
                    type = WEAK_EN
                else if (type == WEAK_CS) then
                    if (it%previous%type == WEAK_AN .and. (type_of_next == WEAK_AN .or. &
                                                           (type_of_next == WEAK_EN .and. it%last_strong%type == STRONG_AL))) &
                        then
                        type = WEAK_AN
                    else if (it%previous%type == WEAK_EN .and. type_of_next == WEAK_EN .and. &
                             it%last_strong%type /= STRONG_AL) then
                        type = WEAK_EN
                    end if
                end if
            else if (type == WEAK_ET .or. type == WEAK_BN) then
                ! W5 and BN that is part of a sequence of ETs
                if (it%previous%type == WEAK_EN) then
                    type = WEAK_EN
                else if (it%next_en_position > it%character_position .and. it%next_en_type /= WEAK_BN) then
                    if (it%next_en_type == WEAK_EN) type = WEAK_EN
                else if (type == WEAK_BN .and. new_level == 0 .and. .not. bidi_explicit_directory_character(it%ch) .and. &
                         ((it%last_strong%type == STRONG_L) .or. &
                          (it%last_strong%type == UNKNOWN_BT .and. it%sos == L2R))) then
                    ! A BN at level 0 after L stays L.
                    type = STRONG_L
                else if (it%next_en_position >= 0) then
                    ! Look for the first character that is not ET or BN.
                    en_position = it%character_position + it%nchars

                    if (it%character_position + it%nchars >= end_of_buffer) then
                        next_character = BIDI_END_OF_BUFFER
                    else
                        call bidi_character_at_position(it, it%byte_position + it%ch_length, next_character, len)
                    end if

                    type_of_next = bidi_get_type(next_character, override)

                    if (type_of_next == WEAK_ET .or. type_of_next == WEAK_BN .or. bidi_explicit_directory_character( &
                        next_character)) then
                        call bidi_copy_it(saved_it, it)

                        do while (bidi_resolve_explicit(it) == new_level .and. &
                                  (it%type == WEAK_BN .or. it%type == WEAK_ET))
                            type_of_next = it%type
                        end do

                        if (type == WEAK_BN .and. it%character_position == saved_it%character_position + saved_it%nchars) then
                            ! A BN followed by a non-ET, non-BN.
                            en_position = saved_it%character_position
                            type_of_next = type
                        else
                            en_position = it%character_position
                        end if

                        call bidi_copy_it(it, saved_it)
                    end if

                    ! Remember this position, to speed up processing of
                    ! the next ETs.
                    it%next_en_position = en_position

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
                        it%next_en_position = -1
                    end if

                    it%next_en_type = type_of_next
                end if
            end if
        end if

        ! W6
        if (type == WEAK_ES .or. type == WEAK_ET .or. type == WEAK_CS .or. &
            (type == WEAK_BN .and. (it%previous%type == WEAK_CS .or. it%previous%type == WEAK_ES .or. &
                                    it%previous%type == WEAK_ET))) type = NEUTRAL_ON
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

    !> The type of a neutral between PREVIOUS_TYPE and NEXT_TYPE at level LEV
    !> (rules N1 and N2 of UAX#9).  Port of bidi.c:bidi_resolve_neutral_1.
    pure integer function bidi_resolve_neutral_1(previous_type_in, next_type_in, lev) result(type)
        integer, intent(in) :: previous_type_in, next_type_in, lev
        integer :: previous_type, next_type

        previous_type = previous_type_in
        next_type = next_type_in
        ! N1: European and Arabic numbers act as if they were R.
        if (next_type == WEAK_EN .or. next_type == WEAK_AN) next_type = STRONG_R
        if (previous_type == WEAK_EN .or. previous_type == WEAK_AN) previous_type = STRONG_R

        if (next_type == previous_type) then
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
        integer(int32) :: close_character(0:MAXIMUM_BPA_STACK - 1), curchar
        integer(int64) :: open_index(0:MAXIMUM_BPA_STACK - 1), pairing_position, idx_at_entry, end_of_buffer
        integer :: flags(0:MAXIMUM_BPA_STACK - 1)
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
        pairing_position = -1
        idx_at_entry = bidi_cache_idx
        call bidi_copy_it(saved_it, it)
        ! The iterator for fetching cached states is always forward.
        tem_it%scan_directory = 1

        do
            current_level = it%level_stack(it%stack_idx)%level
            if (maxlevel < current_level) maxlevel = current_level
            ! Mark the opening bracket as being resolved (bracket_pairing_position).
            if (btype == BIDI_BRACKET_OPEN .and. it%bracket_pairing_position == -1) it%bracket_pairing_position = &
                it%character_position

            if (.not. bidi_cache_iterator_state(it, type == NEUTRAL_B, .false.)) then
                ! No room in the cache: give up on the BPA.
                call bidi_cache_reset_to(idx_at_entry - bidi_cache_start)
                call bidi_copy_it(it, saved_it)
                return
            end if

            if (btype == BIDI_BRACKET_OPEN) then
                ! PUSH_BPA_STACK
                if (bpa_sp < MAXIMUM_BPA_STACK - 1) then
                    bpa_sp = bpa_sp + 1
                    close_character(bpa_sp) = bidi_mirror_character(canonical_equ(it%ch))
                    open_index(bpa_sp) = bidi_cache_last_idx
                    flags(bpa_sp) = 0
                end if
            else if (btype == BIDI_BRACKET_CLOSE) then
                sp = bpa_sp
                curchar = canonical_equ(it%ch)

                do while (sp >= 0)
                    if (close_character(sp) == curchar) exit
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

                    tem_it%bracket_pairing_position = it%character_position
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
                pairing_position = it%character_position
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
        type = bidi_cache_find(saved_it%character_position, .false., it)

        ! The BPA is unnecessary if the sequence has only characters of
        ! the base direction at the base level.
        if (maxlevel == base_level .and. (l2r_seen .or. r2l_seen) .and. &
            ((base_level == 0 .and. .not. r2l_seen) .or. (base_level == 1 .and. .not. l2r_seen))) then
            end_of_buffer = text_end(it)
            if (retval) pairing_position = it%bracket_pairing_position
            ! The special value END_OF_BUFFER says the BPA was not needed.
            it%bracket_pairing_position = end_of_buffer
            it%bracket_enclosed_type = embedding_type
            call bidi_cache_reset_to(bidi_cache_last_idx - bidi_cache_start)
            it%next_for_neutral%type = embedding_type
            it%next_for_neutral%character_position = pairing_position
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
                    bidi_cache(index)%previous_for_neutral = info
                end if

                exit
            end if
        end do
    end subroutine bidi_record_type_for_neutral

    !> Resolve the type of the next character with the paired brackets
    !> (rule N0 of UAX#9).  Port of bidi.c:bidi_resolve_brackets.
    integer function bidi_resolve_brackets(it) result(type)
        type(bidi_it_type), intent(inout) :: it
        integer :: previous_level, embedding_level, embedding_type, previous_type_for_neutral
        logical :: resolve_bracket
        integer(int32) :: ch
        integer(int64) :: end_of_buffer, index
        type(bidi_saved_info_type) :: previous_for_neutral, next_for_neutral

        previous_level = it%level_stack(it%stack_idx)%level
        resolve_bracket = .false.
        type = UNKNOWN_BT
        ch = 0
        end_of_buffer = text_end(it)

        if (it%type == STRONG_L .or. it%type == STRONG_R .or. it%type == WEAK_AN .or. it%type == WEAK_EN) then
            call bidi_remember_character(previous_for_neutral, it, .true.)
        else
            previous_for_neutral = it%previous_for_neutral
        end if

        ! The next character for resolving neutrals, if still ahead.
        if (it%next_for_neutral%character_position > it%character_position) then
            next_for_neutral = it%next_for_neutral
        else
            next_for_neutral%character_position = -1
        end if

        if (.not. it%first_element) then
            type = bidi_cache_find(it%character_position + it%nchars, .false., it)
            ch = it%ch
        end if

        if (type == UNKNOWN_BT) then
            type = bidi_resolve_weak(it)

            if (type == NEUTRAL_ON) then
                ! The BPA was found unnecessary for this sequence: a closing
                ! bracket takes the type of the run.
                if (it%bracket_pairing_position == end_of_buffer) then
                    if (it%next_for_neutral%character_position == it%character_position .and. &
                        bidi_paired_bracket_type(it%ch) == BIDI_BRACKET_CLOSE) type = it%bracket_enclosed_type
                else if (bidi_find_bracket_pairs(it)) then
                    resolve_bracket = .true.
                end if
            end if
        else if (it%bracket_pairing_position /= end_of_buffer) then
            ! The cached state of an isolate's first character carries its
            ! own neutral context.
            if (it%level_stack(it%stack_idx)%level > previous_level .and. isolate_status(it, it%stack_idx)) then
                call bidi_record_type_for_neutral(previous_for_neutral, previous_level, .false.)
                call bidi_record_type_for_neutral(next_for_neutral, previous_level, .true.)
            end if

            if (type == NEUTRAL_ON .and. bidi_paired_bracket_type(ch) == BIDI_BRACKET_OPEN) then
                if (it%bracket_pairing_position > it%character_position) then
                    ! A cached opening bracket with a known pair.
                    resolve_bracket = .true.
                else if (it%bracket_pairing_position == -1) then
                    ! Not yet resolved: look for its pair.
                    if (bidi_find_bracket_pairs(it)) resolve_bracket = .true.
                end if
            end if

            ! Keep the neutral context of the level run.
            if (it%level_stack(it%stack_idx)%level == previous_level) then
                it%previous_for_neutral = previous_for_neutral
                if (next_for_neutral%character_position > 0) it%next_for_neutral = next_for_neutral
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
                previous_type_for_neutral = it%previous_for_neutral%type
                if (previous_type_for_neutral == UNKNOWN_BT) previous_type_for_neutral = embedding_type

                select case (previous_type_for_neutral)
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
                index = bidi_cache_search(it%bracket_pairing_position, -1, 1)
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

        if (.not. ((type /= NEUTRAL_B .and. (is_neutral .or. bidi_isolate_fmt_character(type))) .or. type == WEAK_BN)) then
            return
        end if

        if (it%next_for_neutral%type /= UNKNOWN_BT .and. &
            (it%next_for_neutral%character_position > it%character_position .or. &
             (it%next_for_neutral%character_position == it%character_position .and. it%type == PDI))) then
            ! The next strong character is already known.
            type = bidi_resolve_neutral_1(it%previous_for_neutral%type, it%next_for_neutral%type, current_level)
        else if (current_level == 0 .and. it%previous_for_neutral%type == STRONG_L .and. &
                 (it%ch >= 0 .and. it%ch < 128 .or. (type /= WEAK_BN .and. .not. bidi_explicit_directory_character(it%ch) .and. &
                                                     .not. bidi_isolate_fmt_character(type)))) then
            ! Speed up: neutrals after L at level 0 are L (as N1/N2 make
            ! them, whatever follows).
            type = bidi_resolve_neutral_1(it%previous_for_neutral%type, STRONG_L, current_level)
        else if (current_level == 1 .and. it%level_stack(0)%level == 1 .and. &
                 (it%previous_for_neutral%type == STRONG_R .or. it%previous_for_neutral%type == WEAK_EN .or. &
                  it%previous_for_neutral%type == WEAK_AN) .and. type /= WEAK_BN .and. &
                 .not. bidi_explicit_directory_character(it%ch) .and. .not. bidi_isolate_fmt_character(type)) then
            ! Likewise for neutrals after R at level 1 of an R2L paragraph.
            type = bidi_resolve_neutral_1(it%previous_for_neutral%type, STRONG_R, current_level)
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
                                                      bidi_isolate_fmt_character(type))) adjacent_to_neutrals = .true.
                if (type == NEUTRAL_B) exit
                if (type /= WEAK_BN .and. bidi_get_category(type) /= NEUTRAL .and. .not. bidi_isolate_fmt_character(type)) exit
                if (it%level_stack(it%stack_idx)%level /= current_level) exit
            end do

            ! Record the character that stopped the search.
            call bidi_remember_character(saved_it%next_for_neutral, it, .true.)

            if (it%level_stack(it%stack_idx)%level /= current_level .or. type == NEUTRAL_B) then
                ! The sequence ended: the eos type is the embedding direction.
                if (adjacent_to_neutrals) then
                    next_type = it%previous_for_neutral%type
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

            type = bidi_resolve_neutral_1(saved_it%previous_for_neutral%type, next_type, current_level)
            saved_it%next_for_neutral%type = next_type
            saved_it%type = type
            call bidi_copy_it(it, saved_it)
        end if
    end function bidi_resolve_neutral

    !> The resolved type of the next character.  Port of
    !> bidi.c:bidi_type_of_next_char.
    integer function bidi_type_of_next_character(it) result(type)
        type(bidi_it_type), intent(inout) :: it

        type = bidi_resolve_neutral(it)
    end function bidi_type_of_next_character

    !> Move the iterator to the next character in logical order (or in
    !> the scan direction through the cache) and return its resolved
    !> level (rules I1, I2 and L1 of UAX#9).  Port of
    !> bidi.c:bidi_level_of_next_char.
    integer function bidi_level_of_next_character(it) result(level)
        type(bidi_it_type), intent(inout) :: it
        integer :: type, chtype, dpp
        integer(int64) :: next_character_position, beginning_of_buffer, clen, buffer_position, cpos, disp_position, nc
        integer(int32) :: ch
        logical :: ok

        type = UNKNOWN_BT
        next_character_position = -2

        if (it%scan_directory == 1) then
            ! There's no sense in trying to advance if we've hit the end.
            if (it%character_position >= text_end(it)) then
                level = it%resolved_level
                return
            end if
        end if

        ! Perhaps the character is in the cache.
        if (bidi_cache_idx > bidi_cache_start .and. .not. it%first_element) then
            beginning_of_buffer = 1
            if (it%string%active) beginning_of_buffer = 0

            if (it%scan_directory > 0) then
                next_character_position = it%character_position + it%nchars
            else if (it%character_position >= beginning_of_buffer) then
                ! Previous character is at END_OF_BUFFER if charpos is 0 in a string.
                next_character_position = it%character_position - 1
            end if

            if (next_character_position >= beginning_of_buffer - 1) type = bidi_cache_find(next_character_position, .true., it)

            if (type /= UNKNOWN_BT) then
                level = it%resolved_level
                return
            end if
        end if

        if (type == UNKNOWN_BT) type = bidi_type_of_next_character(it)

        if (type == NEUTRAL_B) then
            level = it%resolved_level
            return
        end if

        level = it%level_stack(it%stack_idx)%level
        it%type = type

        ! L1: whitespace, BNs and isolate formatting characters before a
        ! segment or paragraph separator, or at the end of a line, get the
        ! paragraph level.  Find the character after them.
        if ((it%original_type == NEUTRAL_WHITESPACE .or. (it%original_type == WEAK_BN .and. level /= it%level_stack(0)%level) .or. &
             bidi_isolate_fmt_character(it%original_type)) &
            .and. it%next_for_whitespace%character_position < it%character_position) then
            clen = it%ch_length
            buffer_position = it%byte_position
            cpos = it%character_position
            disp_position = it%disp_position
            nc = it%nchars
            dpp = it%disp_property

            do
                cpos = cpos + nc
                buffer_position = buffer_position + clen
                ch = bidi_fetch_character(it, cpos, buffer_position, disp_position, dpp, clen, nc)
                chtype = bidi_get_type(ch, NEUTRAL_DIRECTORY)
                if (.not. (chtype == NEUTRAL_WHITESPACE .or. chtype == WEAK_BN .or. bidi_isolate_fmt_character(chtype) .or. &
                           bidi_explicit_directory_character(ch))) exit
            end do

            it%next_for_whitespace%type = chtype
            it%next_for_whitespace%character_position = cpos
        end if

        ! Update the cache, but only if this state was already cached.
        ok = bidi_cache_iterator_state(it, .true., .true.)

        ! Resolve implicit levels.
        if (it%original_type == NEUTRAL_B .or. it%original_type == NEUTRAL_S .or. it%ch == 10 .or. it%ch == BIDI_END_OF_BUFFER &
            .or. &
            ((it%original_type == NEUTRAL_WHITESPACE .or. it%original_type == WEAK_BN .or. bidi_isolate_fmt_character( &
              it%original_type) .or. &
              bidi_explicit_directory_character(it%ch)) .and. &
             (it%next_for_whitespace%type == NEUTRAL_B .or. it%next_for_whitespace%type == NEUTRAL_S))) then
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
    end function bidi_level_of_next_character

    !> Move to the other edge of the run at LEVEL or higher: forward
    !> through new characters, or (END_FLAG) back through the cache.
    !> Port of bidi.c:bidi_find_other_level_edge.
    subroutine bidi_find_other_level_edge(it, level, end_flag)
        type(bidi_it_type), intent(inout) :: it
        integer, intent(in) :: level
        logical, intent(in) :: end_flag
        integer :: directory, new_level
        integer(int64) :: index
        logical :: ok

        if (end_flag) then
            directory = -it%scan_directory
        else
            directory = it%scan_directory
        end if

        index = bidi_cache_find_level_change(level, directory, end_flag)

        if (index >= bidi_cache_start) then
            call bidi_cache_fetch_state(index, it)
        else
            ok = bidi_cache_iterator_state(it, .true., .false.)

            do
                new_level = bidi_level_of_next_character(it)

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
        integer :: old_level, new_level, next_level, level_to_search, increment, expected_next_level
        type(bidi_it_type) :: sentinel
        logical :: ascending, ok
        integer(int64) :: separator_length

        if (it%scan_directory == 0) it%scan_directory = 1
        ! If we just passed a newline, initialize for the next line.
        if (.not. it%first_element .and. (it%ch == 10 .or. it%ch == BIDI_END_OF_BUFFER)) call bidi_line_init(it)

        ! Prepare the sentinel iterator state: a newline before the start.
        if (bidi_cache_idx == bidi_cache_start) then
            call bidi_copy_it(sentinel, it)

            if (it%first_element) then
                sentinel%character_position = sentinel%character_position - 1
                sentinel%byte_position = sentinel%byte_position - 1
                sentinel%ch = 10
                sentinel%ch_length = 1
                sentinel%nchars = 1
            end if

            ok = bidi_cache_iterator_state(sentinel, .true., .false.)
        end if

        old_level = it%resolved_level
        new_level = bidi_level_of_next_character(it)

        ! Reordering of resolved levels (clause L2) is implemented by
        ! jumping to the other edge of the level and flipping direction.
        if (new_level /= old_level) then
            ascending = new_level > old_level

            if (ascending) then
                level_to_search = old_level + 1
                increment = 1
            else
                level_to_search = old_level
                increment = -1
            end if

            expected_next_level = old_level + increment
            call bidi_find_other_level_edge(it, level_to_search,.not. ascending)
            it%scan_directory = -it%scan_directory
            next_level = bidi_peek_at_next_level(it)

            do while (next_level /= expected_next_level)
                expected_next_level = expected_next_level + increment
                level_to_search = level_to_search + increment
                call bidi_find_other_level_edge(it, level_to_search,.not. ascending)
                it%scan_directory = -it%scan_directory
                next_level = bidi_peek_at_next_level(it)
            end do

            ! Finally, deliver the next character in the new direction.
            next_level = bidi_level_of_next_character(it)
        end if

        ! Take note when we have just processed the newline that precedes
        ! the end of the paragraph.
        if (it%scan_directory == 1 .and. (it%ch == 10 .or. it%ch == BIDI_END_OF_BUFFER)) then
            if (it%string%active) then
                it%separator_limit = it%string%count_characters
            else if (it%byte_position < buffers(current)%end_of_accessible_portion_byte) then
                separator_length = bidi_at_paragraph_end(it%character_position + it%nchars)

                if (separator_length >= 0) then
                    it%new_paragraph = .true.

                    ! Record the buffer position of the last character of
                    ! the paragraph separator.
                    if (separator_length > 0) then
                        it%separator_limit = it%character_position + it%nchars + separator_length
                    else
                        it%separator_limit = it%character_position
                    end if
                end if
            end if
        end if

        ! The cache is no longer needed when back at the base level after
        ! the cached characters.
        if (it%scan_directory == 1 .and. bidi_cache_idx > bidi_cache_start) then
            if (it%resolved_level == it%level_stack(0)%level .and. &
                it%character_position > bidi_cache(bidi_cache_idx - 1)%character_position + bidi_cache(bidi_cache_idx - 1)%nchars &
                - 1) then
                call bidi_cache_reset()
                ! hazard-ok(short-circuit): bidi_cache_idx > bidi_cache_start >= 0 here, so the index is valid.
            else if (it%resolved_level == it%level_stack(0)%level .and. bidi_cache_idx >= bidi_cache_size .and. &
                     it%character_position == bidi_cache(bidi_cache_idx - 1)%character_position) then
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
    integer(int64) function bidi_find_first_overridden(it) result(found_position)
        type(bidi_it_type), intent(inout) :: it
        integer :: maximum_l2r, maximum_r2l, maximum_weak, type, level, category
        integer(int64) :: end_of_buffer

        end_of_buffer = text_end(it)
        found_position = end_of_buffer
        maximum_r2l = 1

        if (it%paragraph_directory == L2R) then
            maximum_l2r = 0
            maximum_weak = 1
        else
            maximum_l2r = 2
            maximum_weak = 2
        end if

        do
            type = bidi_resolve_weak(it)
            level = it%level_stack(it%stack_idx)%level
            category = bidi_get_category(it%original_type)
            ! Detect when we have overridden directionality, or have
            ! embedding levels larger than needed for the context.
            if ((type == STRONG_R .and. it%original_type == STRONG_L) .or. &
                (type == STRONG_L .and. (it%original_type == STRONG_R .or. it%original_type == STRONG_AL)) .or. &
                ((it%original_type == STRONG_L .or. it%original_type == WEAK_EN) .and. level > maximum_l2r) .or. &
                ((it%original_type == STRONG_R .or. it%original_type == STRONG_AL) .and. level > maximum_r2l) .or. &
                ((category == WEAK .or. it%original_type == NEUTRAL_ON) &
                 .and. level > maximum_weak)) found_position = it%character_position
            if (found_position /= end_of_buffer .or. it%character_position >= end_of_buffer .or. it%ch == BIDI_END_OF_BUFFER &
                .or. it%ch == 10) exit
        end do
    end function bidi_find_first_overridden
end module bidirectional_mod
