! minibuffer_styles_fm.f90 -- completion styles, a submodule of minibuffer_fm.
! Ported from GNU Emacs 31.1 lisp/minibuffer.el (completion styles,
! partial-completion, substring, flex and initials completion) and
! src/minibuf.c (completion--flex-cost-gotoh).
!
! Copyright (C) 2008-2026 Free Software Foundation, Inc.
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

!> Completion styles: `completion_try_completion' and
!> `completion_all_completions' try each of `completion_styles' (those of
!> the table's category first, from `completion_category_overrides' and
!> `completion_category_defaults') until one gives a result.  The styles
!> are emacs21, emacs22, basic, partial-completion, substring, flex and
!> initials, as completion-styles-alist defines them.
!>
!> Patterns of partial-completion are arrays of elements: strings and the
!> wildcards `star', `point', `any', `any-delim' and `prefix'.
!>
!> Deferred: the faces of `completion_pcm__hilit_commonality' (strings
!> carry no text properties), the shorthand style (Lisp symbol shorthands;
!> it never matches here), styles with let-bound variables, the
!> `completion-try-word' property that keeps a delimiter inserted by
!> `minibuffer_complete_word' from being one, the unquote/requote
!> machinery of completion--nth-completion, and `completion-lazy-hilit'.
submodule(minibuffer_fm) minibuffer_styles_fm
    use search_fm, only: string_match, match_beginning, match_end, case_fold_search
    use casetab_fm, only: downcase_char => downcase
    implicit none(type, external)

    ! Pattern elements.
    integer, parameter :: P_STRING = 0, P_STAR = 1, P_POINT = 2, P_ANY = 3, P_ANY_DELIM = 4, P_PREFIX = 5

    type :: pat_elt
        integer :: kind = P_STRING
        character(len=:), allocatable :: s
    end type pat_elt

    !> A segment of a pattern: a fixed string and the wildcards after it.
    type :: segment_type
        character(len=:), allocatable :: fixed
        integer, allocatable :: wildcards(:)
    end type segment_type

contains

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> The strings of the list V.  Not a port.
    subroutine to_boxes(v, boxes)
        type(value_type), intent(in) :: v
        type(string_box), allocatable, intent(out) :: boxes(:)
        type(value_type), allocatable :: elts(:)
        integer :: k

        call list_elements(v, elts)
        allocate (boxes(size(elts)))

        do k = 1, size(elts)
            boxes(k)%s = xstring(elts(k))
        end do
    end subroutine to_boxes

    !> BOXES as a list of strings.  Not a port.
    function from_boxes(boxes) result(v)
        type(string_box), intent(in) :: boxes(:)
        type(value_type) :: v
        type(value_type), allocatable :: elts(:)
        integer :: k

        allocate (elts(size(boxes)))

        do k = 1, size(boxes)
            elts(k) = make_string(boxes(k)%s)
        end do

        v = make_list(elts)
    end function from_boxes

    !> Append S to BOXES.  Not a port.
    subroutine push_box(boxes, s)
        type(string_box), allocatable, intent(inout) :: boxes(:)
        character(len=*), intent(in) :: s
        type(string_box), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(boxes)) allocate (boxes(0))
        n = size(boxes)
        allocate (grown(n + 1))
        grown(1:n) = boxes
        grown(n + 1)%s = s
        call move_alloc(grown, boxes)
    end subroutine push_box

    !> Append an element to the pattern P.  Not a port.
    subroutine push_elt(p, kind, s)
        type(pat_elt), allocatable, intent(inout) :: p(:)
        integer, intent(in) :: kind
        character(len=*), intent(in), optional :: s
        type(pat_elt), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(p)) allocate (p(0))
        n = size(p)
        allocate (grown(n + 1))
        grown(1:n) = p
        grown(n + 1)%kind = kind

        if (present(s)) then
            grown(n + 1)%s = s
        else
            grown(n + 1)%s = ""
        end if

        call move_alloc(grown, p)
    end subroutine push_elt

    !> Whether regexp RE matches STRING (from 1-based index START), with
    !> case folded when `completion_ignore_case'; keeps the match data.
    !> Not a port.
    integer(int64) function re_match(re, string, start) result(idx)
        character(len=*), intent(in) :: re, string
        integer(int64), intent(in), optional :: start
        type(status_type) :: st
        logical :: saved

        saved = case_fold_search%value
        case_fold_search%value = completion_ignore_case%value
        idx = string_match(re, string, st, start)
        case_fold_search%value = saved
        if (.not. st%ok()) idx = 0
    end function re_match

    !> S with the regexp special characters quoted.  Port of
    !> search.c:Fregexp_quote.
    function regexp_quote(s) result(q)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: q
        integer :: i

        q = ""

        do i = 1, len(s)
            if (index("[*.\?+^$", s(i:i)) > 0) q = q//"\"
            q = q//s(i:i)
        end do
    end function regexp_quote

    !> The try-completion value of STRING over the strings BOXES.  Not a port.
    subroutine try_strings(string, boxes, res)
        character(len=*), intent(in) :: string
        type(string_box), intent(in) :: boxes(:)
        type(value_type), intent(out) :: res
        type(status_type) :: st

        call try_completion(string, completion_list_table(from_boxes(boxes)), res, st)
    end subroutine try_strings

    !> Merge end of COMPLETION with beginning of SUFFIX.
    !> Simple generalization of the "merge trailing /" done in Emacs-22.
    !> Return the new suffix.  Port of minibuffer.el:completion--merge-suffix.
    function merge_suffix(completion, point, suffix) result(r)
        character(len=*), intent(in) :: completion, suffix
        integer(int64), intent(in) :: point
        character(len=:), allocatable :: r, tail
        integer(int64) :: i, n

        r = suffix
        if (len(suffix) == 0) return
        n = schars(completion)

        ! The leftmost end of COMPLETION (from POINT on) that SUFFIX starts
        ! with; it must not be empty.
        do i = point, n - 1
            tail = substring(completion, i)

            if (len(tail) <= len(suffix)) then
                if (suffix(1:len(tail)) == tail) then
                    r = suffix(len(tail) + 1:)
                    return
                end if
            end if
        end do
    end function merge_suffix

    !> The `try_completion' value `v' as a try result.  Not a port.
    subroutine set_try_result(v, res)
        type(value_type), intent(in) :: v
        type(try_result_type), intent(out) :: res

        if (stringp(v)) then
            res%kind = TRY_STRING
            res%string = xstring(v)
        else if (nilp(v)) then
            res%kind = TRY_NONE
        else
            res%kind = TRY_EXACT
        end if
    end subroutine set_try_result

    !> The defaults of the completion style settings not set yet.
    module procedure completion_settings_defaults
    if (.not. allocated(completion_styles)) then
        completion_styles = [character(len=24) :: "basic", "partial-completion", "emacs22"]
    end if

    if (.not. allocated(completion_category_defaults)) then
        allocate (completion_category_defaults(6))
        call set_category(completion_category_defaults(1), "buffer", [character(len=24) :: "basic", "substring"])
        call set_category(completion_category_defaults(2), "unicode-name", &
                          [character(len=24) :: "basic", "substring"])
        call set_category(completion_category_defaults(3), "project-file", [character(len=24) :: "substring"])
        call set_category(completion_category_defaults(4), "xref-location", [character(len=24) :: "substring"])
        call set_category(completion_category_defaults(5), "info-menu", [character(len=24) :: "basic", "substring"])
        call set_category(completion_category_defaults(6), "symbol-help", &
                          [character(len=24) :: "basic", "shorthand", "substring"])
    end if

    if (.not. allocated(completion_category_overrides)) allocate (completion_category_overrides(0))
    if (.not. allocated(completion_pcm_word_delimiters)) completion_pcm_word_delimiters = "-_./:| "

contains

    subroutine set_category(c, name, styles)
        type(completion_category_type), intent(out) :: c
        character(len=*), intent(in) :: name
        character(len=24), intent(in) :: styles(:)

        c%category = name
        c%styles = styles
    end subroutine set_category
    end procedure completion_settings_defaults

    ! ------------------------------------------------------------------
    ! Dispatch
    ! ------------------------------------------------------------------

    !> The styles to use for TABLE: those its category's
    !> `completion_category_overrides' or `completion_category_defaults'
    !> entry gives first, then `completion_styles', without duplicates.
    !> Port of minibuffer.el:completion--styles.
    subroutine styles_for(table, styles)
        type(completion_table_type), intent(in) :: table
        character(len=24), allocatable, intent(out) :: styles(:)
        character(len=24), allocatable :: over(:)
        integer :: k, n

        call completion_settings_defaults()

        if (allocated(table%category)) then
            call category_styles(table%category, over)
        else
            allocate (over(0))
        end if

        allocate (styles(size(over) + size(completion_styles)))
        n = 0

        do k = 1, size(over)
            if (any(styles(1:n) == over(k))) cycle
            n = n + 1
            styles(n) = over(k)
        end do

        do k = 1, size(completion_styles)
            if (any(styles(1:n) == completion_styles(k))) cycle
            n = n + 1
            styles(n) = completion_styles(k)
        end do

        styles = styles(1:n)
    end subroutine styles_for

    !> The `styles' property of category CAT, from
    !> `completion_category_overrides' then `completion_category_defaults'.
    !> Port of minibuffer.el:completion-category-get.
    subroutine category_styles(cat, styles)
        character(len=*), intent(in) :: cat
        character(len=24), allocatable, intent(out) :: styles(:)
        integer :: k

        allocate (styles(0))
        if (len(cat) == 0) return

        do k = 1, size(completion_category_overrides)
            if (trim(completion_category_overrides(k)%category) == cat) then
                styles = completion_category_overrides(k)%styles
                return
            end if
        end do

        do k = 1, size(completion_category_defaults)
            if (trim(completion_category_defaults(k)%category) == cat) then
                styles = completion_category_defaults(k)%styles
                return
            end if
        end do
    end subroutine category_styles

    module procedure completion_try_completion
    character(len=24), allocatable :: styles(:)
    integer :: k

    call styles_for(table, styles)

    do k = 1, size(styles)
        select case (trim(styles(k)))
        case ("emacs21")
            call emacs21_try_completion(string, table, res, pred)
        case ("emacs22")
            call emacs22_try_completion(string, table, point, res, pred)
        case ("basic")
            call basic_try_completion(string, table, point, res, pred)
        case ("partial-completion")
            call pcm_try_completion(string, table, point, res, pred)
        case ("substring")
            call substring_try_completion(string, table, point, res, pred)
        case ("flex")
            call flex_try_completion(string, table, point, res, pred)
        case ("initials")
            call initials_try_completion(string, table, res, pred)
        case default
            ! shorthand: no shorthands here.
            res = try_result_type()
        end select

        if (res%kind /= TRY_NONE) return
    end do
    end procedure completion_try_completion

    module procedure completion_all_completions
    character(len=24), allocatable :: styles(:)
    integer :: k
    logical :: found

    call styles_for(table, styles)
    completion_flex_sorting = .false.
    all = nil()
    base_size = 0

    do k = 1, size(styles)
        found = .false.

        select case (trim(styles(k)))
        case ("emacs21")
            call emacs21_all_completions(string, table, all, base_size, pred)
        case ("emacs22")
            call emacs22_all_completions(string, table, point, all, base_size, pred)
        case ("basic")
            call basic_all_completions(string, table, point, all, base_size, pred)
        case ("partial-completion")
            call pcm_all_completions(string, table, point, all, base_size, pred)
        case ("substring")
            call substring_all_completions(string, table, point, all, base_size, pred)
        case ("flex")
            call flex_all_completions(string, table, point, all, base_size, pred)
            ! completion--flex-adjust-metadata: sort by cost when flex
            ! filters.
            if (list_length(all) > 0) completion_flex_sorting = len(flex_pattern_str) > 0
        case ("initials")
            call initials_all_completions(string, table, all, base_size, pred)
        case default
            continue
        end select

        if (list_length(all) > 0) return
    end do
    end procedure completion_all_completions

    ! ------------------------------------------------------------------
    ! emacs21, emacs22, basic
    ! ------------------------------------------------------------------

    !> Port of minibuffer.el:completion-emacs21-try-completion.
    subroutine emacs21_try_completion(string, table, res, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        type(try_result_type), intent(out) :: res
        procedure(completion_predicate), optional :: pred
        type(value_type) :: completion
        type(status_type) :: status

        call try_completion(string, table, completion, status, pred)
        call set_try_result(completion, res)
        if (res%kind == TRY_STRING) res%point = schars(res%string)
    end subroutine emacs21_try_completion

    !> Port of minibuffer.el:completion-emacs21-all-completions.
    subroutine emacs21_all_completions(string, table, all, base_size, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        type(value_type), intent(out) :: all
        integer(int64), intent(out) :: base_size
        procedure(completion_predicate), optional :: pred
        type(status_type) :: status
        integer(int64) :: bstart, bend

        call all_completions(string, table, all, status, pred)
        call completion_boundaries(string, table, "", bstart, bend, pred)
        base_size = bstart
    end subroutine emacs21_all_completions

    !> Port of minibuffer.el:completion-emacs22-try-completion.
    subroutine emacs22_try_completion(string, table, point, res, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(try_result_type), intent(out) :: res
        procedure(completion_predicate), optional :: pred
        character(len=:), allocatable :: suffix
        type(value_type) :: completion
        type(status_type) :: status

        suffix = substring(string, point)
        call try_completion(substring(string, 0_int64, point), table, completion, status, pred)
        call set_try_result(completion, res)

        select case (res%kind)
        case (TRY_EXACT)
            if (len(suffix) > 0) then
                res%kind = TRY_STRING
                res%string = string
                res%point = point
            end if
        case (TRY_STRING)
            ! Merge a trailing / in completion with a / after point.
            if (len(res%string) > 0 .and. len(suffix) > 0) then
                if (res%string(len(res%string):) == "/" .and. suffix(1:1) == "/") suffix = suffix(2:)
            end if

            res%point = schars(res%string)
            res%string = res%string//suffix
        case default
            ! nil: no completion.
            continue
        end select
    end subroutine emacs22_try_completion

    !> Port of minibuffer.el:completion-emacs22-all-completions.
    subroutine emacs22_all_completions(string, table, point, all, base_size, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(value_type), intent(out) :: all
        integer(int64), intent(out) :: base_size
        procedure(completion_predicate), optional :: pred
        character(len=:), allocatable :: beforepoint
        type(status_type) :: status
        integer(int64) :: bstart, bend

        beforepoint = substring(string, 0_int64, point)
        call all_completions(beforepoint, table, all, status, pred)
        call completion_boundaries(beforepoint, table, "", bstart, bend, pred)
        base_size = bstart
    end subroutine emacs22_all_completions

    !> The pattern of basic completion: the field before point, point,
    !> and the field after point.  Port of
    !> minibuffer.el:completion-basic--pattern.
    subroutine basic_pattern(beforepoint, afterpoint, bstart, bend, p)
        character(len=*), intent(in) :: beforepoint, afterpoint
        integer(int64), intent(in) :: bstart, bend
        type(pat_elt), allocatable, intent(out) :: p(:)

        allocate (p(0))
        call push_elt(p, P_STRING, substring(beforepoint, bstart))
        call push_elt(p, P_POINT)
        call push_elt(p, P_STRING, substring(afterpoint, 0_int64, bend))
    end subroutine basic_pattern

    !> Port of minibuffer.el:completion-basic-try-completion.
    subroutine basic_try_completion(string, table, point, res, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(try_result_type), intent(out) :: res
        procedure(completion_predicate), optional :: pred
        character(len=:), allocatable :: beforepoint, afterpoint, suffix, prefix
        type(value_type) :: completion
        type(status_type) :: status
        type(pat_elt), allocatable :: pattern(:)
        type(string_box), allocatable :: all(:)
        integer(int64) :: bstart, bend

        beforepoint = substring(string, 0_int64, point)
        afterpoint = substring(string, point)
        call completion_boundaries(beforepoint, table, afterpoint, bstart, bend, pred)

        if (bend == 0) then
            ! `try_completion' may return a subtly different result than
            ! `all+merge', so try to use it whenever possible.
            call try_completion(beforepoint, table, completion, status, pred)
            call set_try_result(completion, res)

            if (res%kind == TRY_STRING) then
                res%point = schars(res%string)
                res%string = res%string//merge_suffix(res%string, point, afterpoint)
            end if

            return
        end if

        suffix = substring(afterpoint, bend)
        prefix = substring(beforepoint, 0_int64, bstart)
        call basic_pattern(beforepoint, afterpoint, bstart, bend, pattern)
        call optimize_pattern(pattern)
        call pcm_all(prefix, pattern, table, all, pred)
        if (minibuffer_completing_file_name) call filename_try_filter(all)
        call merge_try(pattern, all, prefix, suffix, res)
    end subroutine basic_try_completion

    !> Port of minibuffer.el:completion-basic-all-completions.
    subroutine basic_all_completions(string, table, point, all, base_size, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(value_type), intent(out) :: all
        integer(int64), intent(out) :: base_size
        procedure(completion_predicate), optional :: pred
        character(len=:), allocatable :: beforepoint, afterpoint, prefix
        type(pat_elt), allocatable :: pattern(:)
        type(string_box), allocatable :: boxes(:)
        integer(int64) :: bstart, bend

        beforepoint = substring(string, 0_int64, point)
        afterpoint = substring(string, point)
        call completion_boundaries(beforepoint, table, afterpoint, bstart, bend, pred)
        prefix = substring(beforepoint, 0_int64, bstart)
        call basic_pattern(beforepoint, afterpoint, bstart, bend, pattern)
        call delete_empty(pattern)
        call pcm_all(prefix, pattern, table, boxes, pred)
        all = from_boxes(boxes)
        base_size = bstart
    end subroutine basic_all_completions

    ! ------------------------------------------------------------------
    ! Patterns
    ! ------------------------------------------------------------------

    !> Remove the empty strings of P.  Not a port.
    subroutine delete_empty(p)
        type(pat_elt), allocatable, intent(inout) :: p(:)
        type(pat_elt), allocatable :: q(:)
        integer :: k

        allocate (q(0))

        do k = 1, size(p)
            if (p(k)%kind == P_STRING .and. len(p(k)%s) == 0) cycle
            call push_elt(q, p(k)%kind, p(k)%s)
        end do

        call move_alloc(q, p)
    end subroutine delete_empty

    !> Port of minibuffer.el:completion-pcm--optimize-pattern.
    subroutine optimize_pattern(p)
        type(pat_elt), allocatable, intent(inout) :: p(:)
        type(pat_elt), allocatable :: n(:)
        integer :: k

        ! Remove empty strings in a separate phase since otherwise a ""
        ! might prevent some other optimization, as in '(any "" any).
        call delete_empty(p)
        allocate (n(0))
        k = 1

        do while (k <= size(p))
            if (k < size(p)) then
                if ((p(k)%kind == P_ANY .or. p(k)%kind == P_ANY_DELIM) .and. &
                    (p(k + 1)%kind == P_ANY .or. p(k + 1)%kind == P_POINT)) then
                    k = k + 1
                    cycle
                end if
            end if

            call push_elt(n, p(k)%kind, p(k)%s)
            k = k + 1
        end do

        call move_alloc(n, p)
    end subroutine optimize_pattern

    !> Port of minibuffer.el:completion-pcm--pattern-trivial-p.
    logical function is_pattern_trivial(p)
        type(pat_elt), intent(in) :: p(:)
        integer :: k

        is_pattern_trivial = .false.
        if (size(p) == 0) return
        if (p(1)%kind /= P_STRING) return

        ! It can be followed by `point' and "" and still be trivial.
        do k = 2, size(p)
            if (p(k)%kind == P_POINT) cycle
            if (p(k)%kind == P_STRING .and. len(p(k)%s) == 0) cycle
            return
        end do

        is_pattern_trivial = .true.
    end function is_pattern_trivial

    !> The regexp matching a delimiter of partial-completion.  Port of
    !> minibuffer.el:completion-pcm--delim-re.
    function delim_re(delims) result(re)
        character(len=*), intent(in) :: delims
        character(len=:), allocatable :: re

        re = "["//delims//"*]"
    end function delim_re

    !> Split STRING into a pattern, with `point' at POINT (zero-based;
    !> negative for none).  Port of
    !> minibuffer.el:completion-pcm--string->pattern.
    recursive subroutine string_to_pattern(string, point, pattern)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: point
        type(pat_elt), allocatable, intent(out) :: pattern(:)
        type(pat_elt), allocatable :: tail(:)
        character(len=:), allocatable :: re
        integer(int64) :: p, p0, n, mb1, me0
        integer :: pending, k
        type(status_type) :: st

        allocate (pattern(0))
        n = schars(string)

        if (point >= 0 .and. point <= n) then
            call string_to_pattern(substring(string, 0_int64, point), -1_int64, pattern)
            call push_elt(pattern, P_POINT)
            call string_to_pattern(substring(string, point), -1_int64, tail)

            do k = 1, size(tail)
                call push_elt(pattern, tail(k)%kind, tail(k)%s)
            end do

            return
        end if

        re = delim_re(completion_pcm_word_delimiters)
        p0 = 0
        pending = -1
        p = 0

        do
            ! Indices of string_match are 1-based.
            p = string_match(re, string, st, p + 1, inhibit_modify=.false.)
            if (p == 0 .or. .not. st%ok()) exit
            p = p - 1
            mb1 = match_end(1, st) - 1
            me0 = match_end(0, st) - 1
            ! A submatch 1 delimits a prefix after which something can be
            ! added (completion-pcm--delim-wild-regex with a group).
            if (mb1 >= 0) p = mb1

            if (p0 /= p) then
                if (pending >= 0) call push_elt(pattern, pending)
                call push_elt(pattern, P_STRING, substring(string, p0, p))
            end if

            pending = -1

            if (substring(string, p, p + 1) == "*") then
                call push_elt(pattern, P_STAR)
                p0 = p + 1
            else
                call push_elt(pattern, P_ANY)

                if (mb1 >= 0) then
                    p0 = p
                else
                    call push_elt(pattern, P_STRING, substring(string, p, me0))

                    ! `any-delim' is used so that "a-b" also finds
                    ! "array->beginning".
                    if (completion_pcm_leading_wildcard) then
                        pending = P_PREFIX
                    else
                        pending = P_ANY_DELIM
                    end if

                    p0 = me0
                end if
            end if

            p = p0
            if (p >= n) exit
        end do

        if (n > p0) then
            if (pending >= 0) call push_elt(pattern, pending)
            call push_elt(pattern, P_STRING, substring(string, p0))
        end if

        if (completion_pcm_leading_wildcard .and. size(pattern) > 0) then
            if (pattern(1)%kind == P_STRING) then
                allocate (tail(0))
                call push_elt(tail, P_PREFIX)

                do k = 1, size(pattern)
                    call push_elt(tail, pattern(k)%kind, pattern(k)%s)
                end do

                call move_alloc(tail, pattern)
            end if
        end if
    end subroutine string_to_pattern

    !> Segment PATTERN into its fixed strings and the wildcards after
    !> each.  Port of minibuffer.el:completion-pcm--pattern->segments.
    subroutine pattern_to_segments(pattern, segs)
        type(pat_elt), intent(in) :: pattern(:)
        type(segment_type), allocatable, intent(out) :: segs(:)
        type(segment_type), allocatable :: grown(:)
        type(segment_type) :: seg
        integer :: k, n

        allocate (segs(0))
        k = 1

        do while (k <= size(pattern))
            seg%fixed = ""
            if (allocated(seg%wildcards)) deallocate (seg%wildcards)
            allocate (seg%wildcards(0))

            ! Pop strings from PATTERN and concatenate them.
            do while (k <= size(pattern))
                if (pattern(k)%kind /= P_STRING) exit
                seg%fixed = seg%fixed//pattern(k)%s
                k = k + 1
            end do

            ! Pop wildcards from PATTERN.
            do while (k <= size(pattern))
                if (pattern(k)%kind == P_STRING) exit
                seg%wildcards = [seg%wildcards, pattern(k)%kind]
                k = k + 1
            end do

            n = size(segs)
            allocate (grown(n + 1))
            grown(1:n) = segs
            grown(n + 1) = seg
            call move_alloc(grown, segs)
        end do
    end subroutine pattern_to_segments

    !> Port of minibuffer.el:completion-pcm--segments->regex.
    function segments_to_regex(segs, group) result(re)
        type(segment_type), intent(in) :: segs(:)
        logical, intent(in) :: group
        character(len=:), allocatable :: re
        integer :: k

        re = "\`"

        do k = 1, size(segs)
            re = re//regexp_quote(segs(k)%fixed)

            if (size(segs(k)%wildcards) > 0) then
                if (group) re = re//"\("

                if (all(segs(k)%wildcards == P_ANY_DELIM)) then
                    re = re//delim_re(completion_pcm_word_delimiters)//"*?"
                else
                    re = re//"[^z-a]*?"
                end if

                if (group) re = re//"\)"
            end if
        end do
    end function segments_to_regex

    !> Port of minibuffer.el:completion-pcm--pattern->regex.
    function pattern_to_regex(pattern, group) result(re)
        type(pat_elt), intent(in) :: pattern(:)
        logical, intent(in) :: group
        character(len=:), allocatable :: re
        type(segment_type), allocatable :: segs(:)

        call pattern_to_segments(pattern, segs)
        re = segments_to_regex(segs, group)
    end function pattern_to_regex

    !> Port of minibuffer.el:completion-pcm--pattern->string.
    function pattern_to_string(pattern) result(s)
        type(pat_elt), intent(in) :: pattern(:)
        character(len=:), allocatable :: s
        integer :: k

        s = ""

        do k = 1, size(pattern)
            select case (pattern(k)%kind)
            case (P_STRING)
                s = s//pattern(k)%s
            case (P_STAR)
                s = s//"*"
            case default
                ! any, point, prefix.
                continue
            end select
        end do
    end function pattern_to_string

    !> Find all completions for PATTERN in TABLE obeying PRED.
    !> PATTERN is as returned by `string_to_pattern'.  Port of
    !> minibuffer.el:completion-pcm--all-completions (the regexp filters
    !> the candidates, as completion-regexp-list does).
    subroutine pcm_all(prefix, pattern, table, all, pred)
        character(len=*), intent(in) :: prefix
        type(pat_elt), intent(in) :: pattern(:)
        type(completion_table_type), intent(in) :: table
        type(string_box), allocatable, intent(out) :: all(:)
        procedure(completion_predicate), optional :: pred
        type(value_type) :: compl
        type(string_box), allocatable :: cands(:)
        type(status_type) :: status
        character(len=:), allocatable :: re, head
        integer :: k

        allocate (all(0))

        if (is_pattern_trivial(pattern)) then
            ! Minibuffer contains no delimiters -- simple case!
            call all_completions(prefix//pattern(1)%s, table, compl, status, pred)
            call to_boxes(compl, all)
            return
        end if

        ! Use all-completions to do an initial cull.
        re = pattern_to_regex(pattern, .false.)
        head = ""

        if (size(pattern) > 0) then
            if (pattern(1)%kind == P_STRING) head = pattern(1)%s
        end if

        call all_completions(prefix//head, table, compl, status, pred)
        call to_boxes(compl, cands)

        do k = 1, size(cands)
            if (re_match(re, cands(k)%s) > 0) call push_box(all, cands(k)%s)
        end do
    end subroutine pcm_all

    !> Find all completions for STRING at POINT in TABLE, satisfying PRED:
    !> the pattern, the completions, the prefix and the suffix.  FILTER, if
    !> true, applies `filename_try_filter'.  Port of
    !> minibuffer.el:completion-pcm--find-all-completions.
    recursive subroutine find_all_completions(string_in, table, point, filter, pattern, all, prefix, suffix, pred)
        character(len=*), intent(in) :: string_in
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        logical, intent(in) :: filter
        type(pat_elt), allocatable, intent(out) :: pattern(:)
        type(string_box), allocatable, intent(out) :: all(:)
        character(len=:), allocatable, intent(out) :: prefix, suffix
        procedure(completion_predicate), optional :: pred
        character(len=:), allocatable :: beforepoint, afterpoint, string, sub, subprefix, subsuffix, between, &
                                         newbeforepoint, sep
        type(pat_elt), allocatable :: subpat(:), newpat(:)
        type(string_box), allocatable :: suball(:), kept(:), more(:)
        type(value_type) :: tryv
        type(status_type) :: status
        integer(int64) :: bstart, bend, relpoint, leftbound, nstart, nend
        integer :: k, j

        beforepoint = substring(string_in, 0_int64, point)
        afterpoint = substring(string_in, point)
        call completion_boundaries(beforepoint, table, afterpoint, bstart, bend, pred)
        prefix = substring(beforepoint, 0_int64, bstart)
        suffix = substring(afterpoint, bend)
        string = substring(string_in, bstart, point + bend)
        relpoint = point - bstart
        call string_to_pattern(string, relpoint, pattern)
        call optimize_pattern(pattern)
        call pcm_all(prefix, pattern, table, all, pred)
        if (filter) call filename_try_filter(all)
        if (size(all) > 0 .or. bstart <= 0) return
        call try_completion(prefix, table, tryv, status, pred)
        if (.not. nilp(tryv)) return
        ! The prefix has no completions at all, so we should try and fix
        ! that first.
        sub = substring(prefix, 0_int64, -1_int64)
        call find_all_completions(sub, table, schars(sub), filter, subpat, suball, subprefix, subsuffix, pred)
        sep = substring(prefix, schars(prefix) - 1)
        between = ""

        ! SUBPAT was computed with point at its end; remove the trailing
        ! `point' since that's not the real location of point.
        if (size(subpat) > 0) then
            if (subpat(size(subpat))%kind == P_POINT) subpat = subpat(1:size(subpat) - 1)
        end if

        ! Eliminate submatches that don't end with the separator.
        allocate (kept(0))

        do k = 1, size(suball)
            if (substring(suball(k)%s, schars(suball(k)%s) - 1) == sep) call push_box(kept, suball(k)%s)
        end do

        if (size(kept) > 0) then
            ! Update the boundaries and corresponding pattern.
            newbeforepoint = subprefix//kept(1)%s//substring(string, 0_int64, relpoint)
            leftbound = schars(subprefix) + schars(kept(1)%s)
            call completion_boundaries(newbeforepoint, table, afterpoint, nstart, nend, pred)

            if (.not. ((bend == nend .and. nstart == leftbound) .or. nstart < leftbound)) then
                ! The new completed prefix does change the boundaries of
                ! the completed substring.
                suffix = substring(afterpoint, nend)
                string = substring(newbeforepoint, nstart)//substring(afterpoint, 0_int64, nend)
                between = substring(newbeforepoint, leftbound, nstart)
                call string_to_pattern(string, schars(newbeforepoint) - nstart, pattern)
                call optimize_pattern(pattern)
            end if

            do k = 1, size(kept)
                call pcm_all(subprefix//kept(k)%s//between, pattern, table, more, pred)
                if (filter) call filename_try_filter(more)

                do j = 1, size(more)
                    call push_box(all, kept(k)%s//between//more(j)%s)
                end do
            end do
        end if

        allocate (newpat(0))

        do k = 1, size(subpat)
            call push_elt(newpat, subpat(k)%kind, subpat(k)%s)
        end do

        call push_elt(newpat, P_ANY)
        call push_elt(newpat, P_STRING, sep)
        if (len(between) > 0) call push_elt(newpat, P_STRING, between)

        do k = 1, size(pattern)
            call push_elt(newpat, pattern(k)%kind, pattern(k)%s)
        end do

        call move_alloc(newpat, pattern)
        prefix = subprefix
    end subroutine find_all_completions

    !> Filter to adjust `all' file completion to the behavior of `try':
    !> drop "./", "../" and names with `completion_ignored_extensions',
    !> unless that drops everything.  Port of
    !> minibuffer.el:completion-pcm--filename-try-filter.
    subroutine filename_try_filter(all)
        use dired_fm, only: completion_ignored_extensions
        type(string_box), allocatable, intent(inout) :: all(:)
        type(string_box), allocatable :: try(:)
        integer :: k, j, n
        logical :: ignored
        character(len=:), allocatable :: ext

        if (size(all) == 0) return
        allocate (try(0))

        do k = 1, size(all)
            n = len(all(k)%s)
            ignored = all(k)%s == "./" .or. all(k)%s == "../"

            do j = 1, size(completion_ignored_extensions)
                if (ignored) exit
                ext = trim(completion_ignored_extensions(j))
                if (len(ext) == 0 .or. len(ext) > n) cycle
                ignored = all(k)%s(n - len(ext) + 1:) == ext
            end do

            if (.not. ignored) call push_box(try, all(k)%s)
        end do

        if (size(try) > 0) call move_alloc(try, all)
    end subroutine filename_try_filter

    !> The common suffix of the strings STRS.  Port of
    !> minibuffer.el:completion--common-suffix.
    function common_suffix(strs) result(s)
        type(string_box), intent(in) :: strs(:)
        character(len=:), allocatable :: s
        type(string_box), allocatable :: rev(:)
        type(value_type) :: t
        integer :: k

        allocate (rev(size(strs)))

        do k = 1, size(strs)
            rev(k)%s = reverse_chars(strs(k)%s)
        end do

        call try_strings("", rev, t)
        s = ""
        if (stringp(t)) s = reverse_chars(xstring(t))
    end function common_suffix

    !> S with its characters in reverse order.  Not a port.
    function reverse_chars(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        integer(int64) :: n, k

        n = schars(s)
        r = ""

        do k = n - 1, 0, -1
            r = r//substring(s, k, k + 1)
        end do
    end function reverse_chars

    !> Extract the commonality in STRS, with the help of PATTERN: the
    !> merged pattern, in reverse order.  `any' only grows from the left,
    !> `prefix' only from the right, `star' from both ends (reified as
    !> "*"), `point' like `star' but reified as the position of point.
    !> Port of minibuffer.el:completion-pcm--merge-completions.
    subroutine merge_completions(strs, pattern, res)
        type(string_box), intent(in) :: strs(:)
        type(pat_elt), intent(in) :: pattern(:)
        type(pat_elt), allocatable, intent(out) :: res(:)
        type(pat_elt), allocatable :: pat(:)
        type(segment_type), allocatable :: segs(:)
        type(string_box), allocatable :: comps(:), chopped(:, :), rest(:)
        character(len=:), allocatable :: re, prefix, suf
        type(value_type) :: t
        type(status_type) :: st
        integer(int64) :: last, nxt
        integer :: k, i, j, nparts
        logical :: unique, star_seen, has_prefix_wc, has_grow

        allocate (res(0))

        if (size(strs) <= 1) then
            if (size(strs) == 1) call push_elt(res, P_STRING, strs(1)%s)
            return
        end if

        pat = pattern
        call push_elt(pat, P_ANY)
        call pattern_to_segments(pat, segs)
        ! First chop each string into the parts corresponding to each
        ! non-constant element of `pattern', using regexp-matching.
        re = segments_to_regex(segs, .true.)//"\'"
        nparts = size(segs)
        allocate (chopped(nparts, size(strs)))

        do k = 1, size(strs)
            if (re_match(re, strs(k)%s) == 0) then
                do i = 1, nparts
                    chopped(i, k)%s = ""
                end do

                cycle
            end if

            last = 0

            do i = 1, nparts
                nxt = match_end(i, st) - 1

                if (nxt < 0) then
                    chopped(i, k)%s = ""
                    cycle
                end if

                chopped(i, k)%s = substring(strs(k)%s, last, nxt)
                last = nxt
            end do
        end do

        ! Then for each of those non-constant elements, extract the
        ! commonality between them.
        do i = 1, nparts
            allocate (comps(size(strs)))

            do k = 1, size(strs)
                comps(k)%s = chopped(i, k)%s
            end do

            ! FIXED is a prefix of all of COMPS.  Try to grow that prefix.
            call try_strings(segs(i)%fixed, comps, t)
            unique = .false.

            if (stringp(t)) then
                prefix = xstring(t)
                ! If PREFIX is equal to all of COMPS, then PREFIX is a
                ! unique completion.
                unique = .true.

                do k = 1, size(comps)
                    if (schars(prefix) /= schars(comps(k)%s)) unique = .false.
                end do
            else if (nilp(t)) then
                prefix = segs(i)%fixed
            else
                prefix = segs(i)%fixed
                unique = .true.
            end if

            if (unique) then
                ! If the common prefix is unique, it also is a common
                ! suffix, so we should add it for `prefix' elements.
                call push_elt(res, P_STRING, prefix)
            else
                has_prefix_wc = any(segs(i)%wildcards == P_PREFIX)
                ! `prefix' only wants to include the fixed part before the
                ! wildcard, not the result of growing that fixed part.
                if (has_prefix_wc) prefix = substring(prefix, 0_int64, schars(segs(i)%fixed))
                call push_elt(res, P_STRING, prefix)
                ! Push all the wildcards in this stretch, to preserve
                ! `point' and `star' wildcards before ELEM, collapsing
                ! multiple `star's to one on each side of point.
                star_seen = .false.

                do j = 1, size(segs(i)%wildcards)
                    if (segs(i)%wildcards(j) == P_STAR) then
                        if (.not. star_seen) call push_elt(res, P_STAR)
                        star_seen = .true.
                    else
                        if (segs(i)%wildcards(j) == P_POINT) star_seen = .false.
                        call push_elt(res, segs(i)%wildcards(j))
                    end if
                end do

                ! Extract common suffix additionally to common prefix.
                ! Don't do it for `any' since it could lead to a merged
                ! completion that doesn't itself match the candidates.
                has_grow = any(segs(i)%wildcards == P_STAR .or. segs(i)%wildcards == P_POINT .or. &
                               segs(i)%wildcards == P_PREFIX)

                if (has_grow .and. .not. assoc_string(prefix, comps)) then
                    allocate (rest(size(comps)))

                    do k = 1, size(comps)
                        ! Ignore the chars in the common prefix, so we
                        ! don't merge '("abc" "abbc") as "ab*bc".
                        rest(k)%s = substring(comps(k)%s, schars(prefix))
                    end do

                    suf = common_suffix(rest)
                    if (len(suf) > 0) call push_elt(res, P_STRING, suf)
                    deallocate (rest)
                end if
            end if

            deallocate (comps)
        end do

        ! We return it in reverse order.
        res = res(size(res):1:-1)
    end subroutine merge_completions

    !> Whether S is one of COMPS, ignoring case (assoc-string with
    !> CASE-FOLD).  Not a port.
    logical function assoc_string(s, comps)
        character(len=*), intent(in) :: s
        type(string_box), intent(in) :: comps(:)
        integer :: k

        assoc_string = .false.

        do k = 1, size(comps)
            if (compare_strings(s, comps(k)%s, .true.) == COMPARE_EQUAL) then
                assoc_string = .true.
                return
            end if
        end do
    end function assoc_string

    !> Port of minibuffer.el:completion-pcm--merge-try.
    subroutine merge_try(pattern, all, prefix, suffix_in, res)
        type(pat_elt), intent(in) :: pattern(:)
        type(string_box), intent(in) :: all(:)
        character(len=*), intent(in) :: prefix, suffix_in
        type(try_result_type), intent(out) :: res
        type(pat_elt), allocatable :: merged_pat(:)
        character(len=:), allocatable :: merged, suffix
        integer(int64) :: newpos
        integer :: k, at

        if (size(all) == 0) return

        if (size(all) == 1) then
            ! Ignore completion-ignore-case here.
            if (pattern_to_string(pattern) == all(1)%s) then
                res%kind = TRY_EXACT
                return
            end if
        end if

        call merge_completions(all, pattern, merged_pat)
        ! `merged_pat' is in reverse order.  Place new point (by order of
        ! preference) either at the old point, or at the last place where
        ! there's something to choose, or at the very end.
        at = 0

        do k = 1, size(merged_pat)
            if (merged_pat(k)%kind == P_POINT) then
                at = k
                exit
            end if
        end do

        if (at == 0) then
            do k = 1, size(merged_pat)
                if (merged_pat(k)%kind == P_ANY) then
                    at = k
                    exit
                end if
            end do
        end if

        if (at == 0) then
            do k = 1, size(merged_pat)
                if (merged_pat(k)%kind == P_STAR) then
                    at = k
                    exit
                end if
            end do
        end if

        if (at == 0) at = 1
        ! New pos from the start.
        newpos = schars(pattern_to_string(merged_pat(at:)))
        merged = pattern_to_string(merged_pat(size(merged_pat):1:-1))
        ! The second arg should ideally be "the position right after the
        ! last char of `merged' that comes from the text to be
        ! completed"; the "last but one" position tends to work well in
        ! practice since `suffix' always starts with a boundary.
        suffix = merge_suffix(merged, max(0_int64, schars(merged) - 1), suffix_in)
        res%kind = TRY_STRING
        res%string = prefix//merged//suffix
        res%point = newpos + schars(prefix)
    end subroutine merge_try

    ! ------------------------------------------------------------------
    ! partial-completion, substring
    ! ------------------------------------------------------------------

    !> Port of minibuffer.el:completion-pcm-try-completion.
    subroutine pcm_try_completion(string, table, point, res, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(try_result_type), intent(out) :: res
        procedure(completion_predicate), optional :: pred
        type(pat_elt), allocatable :: pattern(:)
        type(string_box), allocatable :: all(:)
        character(len=:), allocatable :: prefix, suffix

        call find_all_completions(string, table, point, minibuffer_completing_file_name, pattern, all, prefix, &
                                  suffix, pred)
        call merge_try(pattern, all, prefix, suffix, res)
    end subroutine pcm_try_completion

    !> Port of minibuffer.el:completion-pcm-all-completions.
    subroutine pcm_all_completions(string, table, point, all, base_size, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(value_type), intent(out) :: all
        integer(int64), intent(out) :: base_size
        procedure(completion_predicate), optional :: pred
        type(pat_elt), allocatable :: pattern(:)
        type(string_box), allocatable :: boxes(:)
        character(len=:), allocatable :: prefix, suffix

        call find_all_completions(string, table, point, .false., pattern, boxes, prefix, suffix, pred)
        all = from_boxes(boxes)
        base_size = schars(prefix)
    end subroutine pcm_all_completions

    !> Match the presumed substring STRING to the entries in TABLE:
    !> the completions, the pattern, the prefix and the suffix.  Port of
    !> minibuffer.el:completion-substring--all-completions.
    subroutine substring_all(string, table, point, all, pattern, prefix, suffix, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(string_box), allocatable, intent(out) :: all(:)
        type(pat_elt), allocatable, intent(out) :: pattern(:)
        character(len=:), allocatable, intent(out) :: prefix, suffix
        procedure(completion_predicate), optional :: pred
        character(len=:), allocatable :: beforepoint, afterpoint
        type(pat_elt), allocatable :: basic(:)
        integer(int64) :: bstart, bend
        integer :: k

        beforepoint = substring(string, 0_int64, point)
        afterpoint = substring(string, point)
        call completion_boundaries(beforepoint, table, afterpoint, bstart, bend, pred)
        suffix = substring(afterpoint, bend)
        prefix = substring(beforepoint, 0_int64, bstart)
        call basic_pattern(beforepoint, afterpoint, bstart, bend, basic)
        allocate (pattern(0))
        ! basic's pattern always starts with a string.
        call push_elt(pattern, P_PREFIX)

        do k = 1, size(basic)
            call push_elt(pattern, basic(k)%kind, basic(k)%s)
        end do

        call optimize_pattern(pattern)
        call pcm_all(prefix, pattern, table, all, pred)
    end subroutine substring_all

    !> Port of minibuffer.el:completion-substring-try-completion.
    subroutine substring_try_completion(string, table, point, res, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(try_result_type), intent(out) :: res
        procedure(completion_predicate), optional :: pred
        type(pat_elt), allocatable :: pattern(:)
        type(string_box), allocatable :: all(:)
        character(len=:), allocatable :: prefix, suffix

        call substring_all(string, table, point, all, pattern, prefix, suffix, pred)
        if (minibuffer_completing_file_name) call filename_try_filter(all)
        call merge_try(pattern, all, prefix, suffix, res)
    end subroutine substring_try_completion

    !> Port of minibuffer.el:completion-substring-all-completions.
    subroutine substring_all_completions(string, table, point, all, base_size, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(value_type), intent(out) :: all
        integer(int64), intent(out) :: base_size
        procedure(completion_predicate), optional :: pred
        type(pat_elt), allocatable :: pattern(:)
        type(string_box), allocatable :: boxes(:)
        character(len=:), allocatable :: prefix, suffix

        call substring_all(string, table, point, boxes, pattern, prefix, suffix, pred)
        all = from_boxes(boxes)
        base_size = schars(prefix)
    end subroutine substring_all_completions

    ! ------------------------------------------------------------------
    ! flex
    ! ------------------------------------------------------------------

    !> Compute the cost of PAT matching STR using the modified Gotoh
    !> algorithm: COST (lower is better) and MATCHES, where each character
    !> of PAT matched in STR (zero-based); FOUND false if PAT does not
    !> match.  Port of minibuf.c:Fcompletion__flex_cost_gotoh.
    subroutine flex_cost_gotoh(pat, str, found, cost, matches)
        character(len=*), intent(in) :: pat, str
        logical, intent(out) :: found
        integer, intent(out) :: cost
        integer, allocatable, intent(out) :: matches(:)
        integer, parameter :: GAP_OPEN_COST = 10, GAP_EXTEND_COST = 1, POS_INF = huge(0)/2
        integer, parameter :: MAX_STR_SIZE = 512, MAX_PAT_SIZE = 128
        integer, allocatable :: m(:, :), d(:, :), pc(:), sc(:)
        integer :: patlen, strlen, i, j, pi, prev_match, best_cost, lastcol, l
        logical :: match_seen, cmatch

        found = .false.
        cost = 0
        allocate (matches(0))
        call chars_of(pat, pc)
        call chars_of(str, sc)
        patlen = size(pc)
        strlen = size(sc)
        ! Bail if strings are empty or matrix too large.
        if (patlen == 0 .or. strlen == 0 .or. (patlen + 1)*(strlen + 1) > MAX_PAT_SIZE*MAX_STR_SIZE) return

        ! Also bail if PAT is not a subsequence of STR (case-sensitive).
        if (.not. completion_ignore_case%value) then
            pi = 1

            do j = 1, strlen
                if (pi > patlen) exit
                if (sc(j) == pc(pi)) pi = pi + 1
            end do

            if (pi <= patlen) return
        end if

        ! M(i, j) and D(i, j) for i, j from -1.
        allocate (m(-1:patlen - 1, -1:strlen - 1), d(-1:patlen - 1, -1:strlen - 1))
        m = POS_INF
        d = POS_INF
        ! D[-1,-1] is 0 to promote matches at the beginning; the rest of
        ! the first row has GAP_OPEN_COST/2 for cheaper leading gaps.
        d(-1, :) = GAP_OPEN_COST/2
        d(-1, -1) = 0
        prev_match = 0

        do i = 0, patlen - 1
            match_seen = .false.

            do j = prev_match, strlen - 1
                if (completion_ignore_case%value) then
                    cmatch = downcase_char(pc(i + 1)) == downcase_char(sc(j + 1))
                else
                    cmatch = pc(i + 1) == sc(j + 1)
                end if

                if (cmatch) then
                    if (.not. match_seen) then
                        match_seen = .true.
                        prev_match = j
                    end if

                    m(i, j) = min(m(i - 1, j - 1), d(i - 1, j - 1))
                end if

                d(i, j) = min(m(i, j - 1) + GAP_OPEN_COST, d(i, j - 1) + GAP_EXTEND_COST)
            end do
        end do

        ! Find lowest cost in last row.
        best_cost = POS_INF
        lastcol = -1

        do j = 0, strlen - 1
            if (m(patlen - 1, j) < best_cost) then
                best_cost = m(patlen - 1, j)
                lastcol = j
            end if
        end do

        if (lastcol < 0 .or. best_cost >= POS_INF) return
        ! Go backwards to build match positions list.
        deallocate (matches)
        allocate (matches(patlen))
        matches(patlen) = lastcol
        l = lastcol

        do i = patlen - 2, 0, -1
            do
                l = l - 1
                if (l < 0) exit
                if (m(i, l) < d(i, l)) exit
            end do

            matches(i + 1) = l
        end do

        found = .true.
        cost = best_cost
    end subroutine flex_cost_gotoh

    !> The characters of multibyte S.  Not a port.
    subroutine chars_of(s, cs)
        character(len=*), intent(in) :: s
        integer, allocatable, intent(out) :: cs(:)
        integer(int64) :: p
        integer :: c, length, n

        allocate (cs(len(s)))
        n = 0
        p = 1

        do while (p <= len(s, kind=int64))
            call string_char_and_length(s, p, c, length)
            n = n + 1
            cs(n) = c
            p = p + length
        end do

        cs = cs(1:n)
    end subroutine chars_of

    !> Filter the completions of TABLE by PAT, a "flex" string: the
    !> completions (with their costs in `flex_costs'), the pattern
    !> string, the prefix and the suffix.  Port of
    !> minibuffer.el:completion--flex-all-completions-1 with
    !> completion--flex-cost.
    subroutine flex_all_1(pat, table, point, all, costs, pat2, prefix, suffix, pred)
        character(len=*), intent(in) :: pat
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(string_box), allocatable, intent(out) :: all(:)
        integer, allocatable, intent(out) :: costs(:)
        character(len=:), allocatable, intent(out) :: pat2, prefix, suffix
        procedure(completion_predicate), optional :: pred
        character(len=:), allocatable :: beforepoint, afterpoint
        type(value_type) :: v
        type(string_box), allocatable :: cands(:)
        type(status_type) :: status
        integer(int64) :: bstart, bend
        integer, allocatable :: matches(:)
        integer :: k, cost
        logical :: found

        beforepoint = substring(pat, 0_int64, point)
        afterpoint = substring(pat, point)
        call completion_boundaries(beforepoint, table, afterpoint, bstart, bend, pred)
        prefix = substring(beforepoint, 0_int64, bstart)
        suffix = substring(afterpoint, bend)
        pat2 = substring(pat, bstart, point + bend)
        call all_completions(prefix, table, v, status, pred)
        call to_boxes(v, cands)
        allocate (all(0), costs(0))

        if (len(pat2) == 0) then
            call move_alloc(cands, all)
            allocate (costs(size(all)))
            deallocate (costs)
            allocate (costs(size(all)))
            costs = 0
            return
        end if

        do k = 1, size(cands)
            call flex_cost_gotoh(pat2, cands(k)%s, found, cost, matches)
            if (.not. found) cycle
            call push_box(all, cands(k)%s)
            ! completion--flex-cost: (1+ cost) times the extra length.
            costs = [costs, (1 + cost)*int(schars(cands(k)%s) - schars(pat2))]
        end do
    end subroutine flex_all_1

    !> Try to flex-complete STRING in TABLE given PRED and POINT.  Port of
    !> minibuffer.el:completion-flex-try-completion.
    subroutine flex_try_completion(string, table, point, res, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(try_result_type), intent(out) :: res
        procedure(completion_predicate), optional :: pred
        type(string_box), allocatable :: all(:)
        integer, allocatable :: costs(:)
        character(len=:), allocatable :: pat2, prefix, suffix
        type(pat_elt), allocatable :: pattern(:)
        integer(int64) :: point_idx, i

        if (completion_flex_nospace .and. index(string, " ") > 0) return
        call flex_all_1(string, table, point, all, costs, pat2, prefix, suffix, pred)
        allocate (pattern(0))
        call push_elt(pattern, P_PREFIX)
        point_idx = point - schars(prefix) - 1

        do i = 0, schars(pat2) - 1
            call push_elt(pattern, P_STRING, substring(pat2, i, i + 1))

            if (i == point_idx) then
                call push_elt(pattern, P_POINT)
            else
                call push_elt(pattern, P_ANY)
            end if
        end do

        if (minibuffer_completing_file_name) call filename_try_filter(all)
        ! Try some "merging", meaning add as much as possible to the
        ! user's pattern without losing any possible matches in `all'.
        call merge_try(pattern, all, prefix, suffix, res)
    end subroutine flex_try_completion

    !> Get flex-completions of STRING in TABLE, given PRED and POINT.  Port of
    !> minibuffer.el:completion-flex-all-completions (the costs go to
    !> `flex_costs' for the display sorting of
    !> completion--flex-adjust-metadata).
    subroutine flex_all_completions(string, table, point, all, base_size, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(value_type), intent(out) :: all
        integer(int64), intent(out) :: base_size
        procedure(completion_predicate), optional :: pred
        type(string_box), allocatable :: boxes(:)
        integer, allocatable :: costs(:)
        character(len=:), allocatable :: pat2, prefix, suffix
        integer :: k

        all = nil()
        base_size = 0
        if (completion_flex_nospace .and. index(string, " ") > 0) return
        call flex_all_1(string, table, point, boxes, costs, pat2, prefix, suffix, pred)
        flex_pattern_str = pat2
        if (allocated(flex_costs)) deallocate (flex_costs)
        allocate (flex_costs(size(boxes)))

        do k = 1, size(boxes)
            flex_costs(k)%s = boxes(k)%s
            flex_costs(k)%cost = costs(k)
        end do

        all = from_boxes(boxes)
        base_size = schars(prefix)
    end subroutine flex_all_completions

    ! ------------------------------------------------------------------
    ! initials
    ! ------------------------------------------------------------------

    !> STR with a delimiter between each of its characters, if it has no
    !> delimiter itself: "lch" becomes "l-c-h", "~/eee" becomes "~/e/e/e";
    !> "" if initials do not apply.  Port of
    !> minibuffer.el:completion-initials-expand.
    function initials_expand(str, table, pred) result(newstr)
        character(len=*), intent(in) :: str
        type(completion_table_type), intent(in) :: table
        procedure(completion_predicate), optional :: pred
        character(len=:), allocatable :: newstr, without_last, sep
        integer(int64) :: bstart, bend, b2, e2, i, n

        newstr = ""
        call completion_boundaries(str, table, "", bstart, bend, pred)
        n = schars(str)
        if (n == 0) return
        ! Only check within the boundaries, since the boundary char (e.g.
        ! /) might be in delim-regexp.
        if (re_match(delim_re(completion_pcm_word_delimiters), str, bstart + 1) > 0) return

        if (bstart == 0) then
            do i = 0, n - 1
                if (i > 0) newstr = newstr//"-"
                newstr = newstr//substring(str, i, i + 1)
            end do

            return
        end if

        ! If there's a boundary, only when the previous completion field
        ! is empty: ~/eee and /eee and /usr//eee and not much more.
        without_last = substring(str, 0_int64, bstart - 1)
        call completion_boundaries(without_last, table, "", b2, e2, pred)
        if (b2 /= schars(without_last)) return
        sep = substring(str, bstart - 1, bstart)
        newstr = substring(str, 0_int64, bstart)

        do i = bstart, n - 1
            if (i > bstart) newstr = newstr//sep
            newstr = newstr//substring(str, i, i + 1)
        end do
    end function initials_expand

    !> Port of minibuffer.el:completion-initials-all-completions.
    subroutine initials_all_completions(string, table, all, base_size, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        type(value_type), intent(out) :: all
        integer(int64), intent(out) :: base_size
        procedure(completion_predicate), optional :: pred
        character(len=:), allocatable :: newstr

        all = nil()
        base_size = 0
        newstr = initials_expand(string, table, pred)
        if (len(newstr) > 0) call pcm_all_completions(newstr, table, schars(newstr), all, base_size, pred)
    end subroutine initials_all_completions

    !> Port of minibuffer.el:completion-initials-try-completion.
    subroutine initials_try_completion(string, table, res, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        type(try_result_type), intent(out) :: res
        procedure(completion_predicate), optional :: pred
        character(len=:), allocatable :: newstr

        newstr = initials_expand(string, table, pred)
        if (len(newstr) > 0) call pcm_try_completion(newstr, table, schars(newstr), res, pred)
    end subroutine initials_try_completion
end submodule minibuffer_styles_fm
