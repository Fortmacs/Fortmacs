! search_regexp_fm.f90 -- regexp search (the part of search.c that uses
! the regexp engine).
! Ported from GNU Emacs 31.1 src/search.c.
!
! Copyright (C) 1985-1987, 1993-1994, 1997-1999, 2001-2026 Free Software
! Foundation, Inc.
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

!> The cache of compiled patterns, `looking-at`, `string-match` and regexp
!> search in buffers.  A submodule of search_fm, because the regexp engine
!> depends (through the syntax table) on modules that use search_fm.
submodule(search_fm) search_regexp_fm
    use regex_emacs_fm, only: re_pattern_buffer_type, re_registers_type, re_subject_type, re_compile_pattern, &
                              re_search, re_match, subject_from_string, subject_from_buffer
    use syntax_fm, only: syntax_table
    use minibuf_fm, only: read_string
    use callint_fm, only: defcommand
    implicit none(type, external)

    !> Number of compiled patterns kept (REGEXP_CACHE_SIZE).
    integer, parameter :: REGEXP_CACHE_SIZE = 20

    !> A compiled pattern and what it was compiled for (struct
    !> regexp_cache).
    type :: regexp_cache_type
        logical :: valid = .false.
        character(len=:), allocatable :: regexp
        logical :: posix = .false.
        logical :: has_whitespace_regexp = .false.
        character(len=:), allocatable :: whitespace_regexp
        type(handle_type) :: syntax_table
        integer(int64) :: used_at = 0
        type(re_pattern_buffer_type) :: buf
    end type regexp_cache_type

    type(regexp_cache_type), save :: searchbufs(REGEXP_CACHE_SIZE)
    integer(int64) :: clock = 0

contains

    !> Compile PATTERN for case folding `translate` and Posix backtracking
    !> `posix`, or find it already compiled, and return its index in the
    !> cache, moved to the front as most recently used.  Port of
    !> search.c:compile_pattern and compile_pattern_1.
    subroutine compile_pattern(pattern, translate, posix, k, status)
        character(len=*), intent(in) :: pattern
        logical, intent(in) :: translate, posix
        integer, intent(out) :: k
        type(status_type), intent(out) :: status
        integer :: i
        logical :: has_ws
        status = status_type()
        has_ws = allocated(search_spaces_regexp)
        clock = clock + 1
        do i = 1, REGEXP_CACHE_SIZE
            associate (cp => searchbufs(i))
                if (.not. cp%valid) cycle
                if (cp%regexp /= pattern .or. len(cp%regexp) /= len(pattern)) cycle
                if ((cp%buf%translate .neqv. translate) .or. (cp%posix .neqv. posix)) cycle
                if (cp%buf%used_syntax .and. .not. same_handle(cp%syntax_table, syntax_table())) cycle
                if (cp%has_whitespace_regexp .neqv. has_ws) cycle
                if (has_ws) then
                    if (cp%whitespace_regexp /= search_spaces_regexp .or. &
                        len(cp%whitespace_regexp) /= len(search_spaces_regexp)) cycle
                end if
                cp%used_at = clock
                k = i
                return
            end associate
        end do
        ! Compile into the least recently used entry.
        k = minloc(searchbufs%used_at, 1)
        associate (cp => searchbufs(k))
            cp%valid = .false.
            cp%used_at = clock
            cp%buf%translate = translate
            cp%posix = posix
            cp%has_whitespace_regexp = has_ws
            if (has_ws) then
                cp%whitespace_regexp = search_spaces_regexp
                call re_compile_pattern(pattern, cp%buf, status, posix, search_spaces_regexp)
            else
                call re_compile_pattern(pattern, cp%buf, status, posix)
            end if
            ! If the compiled pattern hard codes some of the contents of the
            ! syntax-table, it can only be reused with *this* syntax table.
            cp%syntax_table = syntax_table()
            if (.not. status%ok()) return
            cp%regexp = pattern
            cp%valid = .true.
        end associate
    end subroutine compile_pattern

    !> Whether two handles are the same.
    pure logical function same_handle(a, b)
        type(handle_type), intent(in) :: a, b
        same_handle = a%index == b%index .and. a%generation == b%generation
    end function same_handle

    !> Make the match data from `regs`, offsets in the current buffer's
    !> accessible portion.
    subroutine set_regs_from_buffer(regs)
        type(re_registers_type), intent(in) :: regs
        integer :: i
        call clear_search_regs(size(regs%starts))
        do i = 0, size(regs%starts) - 1
            if (regs%starts(i) >= 0) then
                search_regs_start(i) = byte_to_char(regs%starts(i) + buffers(cur)%begv_byte)
                search_regs_end(i) = byte_to_char(regs%ends(i) + buffers(cur)%begv_byte)
            end if
        end do
        last_thing_searched = current_buffer()
        last_search_in_string = .false.
    end subroutine set_regs_from_buffer

    !> Make the match data from `regs`, offsets in STR.
    subroutine set_regs_from_string(regs, str)
        type(re_registers_type), intent(in) :: regs
        character(len=*), intent(in) :: str
        integer :: i
        call clear_search_regs(size(regs%starts))
        do i = 0, size(regs%starts) - 1
            if (regs%starts(i) >= 0) then
                search_regs_start(i) = string_offset_to_index(str, regs%starts(i))
                search_regs_end(i) = string_offset_to_index(str, regs%ends(i))
            end if
        end do
        last_search_in_string = .true.
    end subroutine set_regs_from_string

    !> Port of search.c:looking_at_1.
    module procedure looking_at_1
    type(re_subject_type) :: subj
    type(re_registers_type) :: regs
    integer(int64) :: i
    integer :: k
    matched = .false.
    call compile_pattern(regexp, case_fold_search, posix, k, status)
    if (.not. status%ok()) return
    subj = subject_from_buffer()
    associate (b => buffers(cur))
        i = re_match(searchbufs(k)%buf, subj, b%pt_byte - b%begv_byte, regs, b%zv_byte - b%begv_byte)
    end associate
    if (i == -2) then
        call matcher_overflow(status)
        return
    end if
    matched = i >= 0
    if (modify_data .and. matched) call set_regs_from_buffer(regs)
    end procedure looking_at_1

    !> Port of search.c:string_match_1.
    module procedure string_match_1
    type(re_subject_type) :: subj
    type(re_registers_type) :: regs
    integer(int64) :: pos, pos_byte, nchars, val
    integer :: k
    idx = 0
    status = status_type()
    pos = 1
    if (present(start)) then
        nchars = string_chars(string)
        pos = start
        if (pos < 0 .and. -pos <= nchars) then
            pos = nchars + pos + 1
        else if (pos < 1 .or. pos > nchars + 1) then
            call set_error(status, FM_ARGS_OUT_OF_RANGE)
            return
        end if
    end if
    pos_byte = string_index_to_offset(string, pos)
    call compile_pattern(regexp, case_fold_search, posix, k, status)
    if (.not. status%ok()) return
    subj = subject_from_string(string)
    val = re_search(searchbufs(k)%buf, subj, pos_byte, len(string, kind=int64) - pos_byte, regs, &
                    len(string, kind=int64))
    if (val == -2) then
        call matcher_overflow(status)
        return
    end if
    if (val < 0) return
    if (modify_data) call set_regs_from_string(regs, string)
    idx = string_offset_to_index(string, val)
    end procedure string_match_1

    !> Regexp search for `search_buffer`.  Port of
    !> search.c:search_buffer_re.
    module procedure search_buffer_re
    type(re_subject_type) :: subj
    type(re_registers_type) :: regs
    integer(int64) :: n, pos, pos_byte, val, begv_byte
    integer :: k
    np = 0
    call compile_pattern(string, fold, posix, k, status)
    if (.not. status%ok()) return
    subj = subject_from_buffer()
    begv_byte = buffers(cur)%begv_byte
    n = n_in
    pos = pos_in
    pos_byte = pos_byte_in
    do while (n < 0)
        ! Don't allow match past current point.
        val = re_search(searchbufs(k)%buf, subj, pos_byte - begv_byte, lim_byte - pos_byte, regs, &
                        pos_byte - begv_byte)
        if (val == -2) then
            call matcher_overflow(status)
            return
        end if
        if (val < 0) then
            np = n
            return
        end if
        pos_byte = regs%starts(0) + begv_byte
        call set_regs_from_buffer(regs)
        ! Set pos to the new position.
        pos = search_regs_start(0)
        n = n + 1
    end do
    do while (n > 0)
        val = re_search(searchbufs(k)%buf, subj, pos_byte - begv_byte, lim_byte - pos_byte, regs, &
                        lim_byte - begv_byte)
        if (val == -2) then
            call matcher_overflow(status)
            return
        end if
        if (val < 0) then
            np = -n
            return
        end if
        pos_byte = regs%ends(0) + begv_byte
        call set_regs_from_buffer(regs)
        pos = search_regs_end(0)
        n = n - 1
    end do
    np = pos
    end procedure search_buffer_re

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> The search commands read their argument in the minibuffer
    !> (interactive "MSearch: " and "sRE search: ").
    subroutine search_cmd(prompt, re, posix, direction, status)
        character(len=*), intent(in) :: prompt
        logical, intent(in) :: re, posix
        integer(int64), intent(in) :: direction
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: string
        integer(int64) :: found
        call read_string(prompt, string, status)
        if (.not. status%ok()) return
        call search_command(string, direction, re, posix, found, status)
    end subroutine search_cmd

    subroutine cmd_search_forward(status)
        type(status_type), intent(out) :: status
        call search_cmd("Search: ", .false., .false., 1_int64, status)
    end subroutine cmd_search_forward

    subroutine cmd_search_backward(status)
        type(status_type), intent(out) :: status
        call search_cmd("Search backward: ", .false., .false., -1_int64, status)
    end subroutine cmd_search_backward

    subroutine cmd_re_search_forward(status)
        type(status_type), intent(out) :: status
        call search_cmd("RE search: ", .true., .false., 1_int64, status)
    end subroutine cmd_re_search_forward

    subroutine cmd_re_search_backward(status)
        type(status_type), intent(out) :: status
        call search_cmd("RE search backward: ", .true., .false., -1_int64, status)
    end subroutine cmd_re_search_backward

    subroutine cmd_posix_search_forward(status)
        type(status_type), intent(out) :: status
        call search_cmd("Posix search: ", .true., .true., 1_int64, status)
    end subroutine cmd_posix_search_forward

    subroutine cmd_posix_search_backward(status)
        type(status_type), intent(out) :: status
        call search_cmd("Posix search backward: ", .true., .true., -1_int64, status)
    end subroutine cmd_posix_search_backward

    module procedure syms_of_search
    call defcommand("search-forward", cmd_search_forward)
    call defcommand("search-backward", cmd_search_backward)
    call defcommand("re-search-forward", cmd_re_search_forward)
    call defcommand("re-search-backward", cmd_re_search_backward)
    call defcommand("posix-search-forward", cmd_posix_search_forward)
    call defcommand("posix-search-backward", cmd_posix_search_backward)
    end procedure syms_of_search
end submodule search_regexp_fm
