! syntax_propertize_fm.f90 -- syntax-table properties and the parse state cache.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/syntax.el.
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

!> Applying `syntax_table' text properties where needed
!> (`syntax_propertize', run lazily by the syntax scanner through
!> `internal_syntax_propertize') and the cached parse state
!> `syntax_ppss'.  Named so as not to clash with syntax_fm (syntax.c).
!>
!> The `syntax_propertize_rules' macro becomes the type
!> `syntax_propertize_rules_type': its RULES are data (a regexp and its
!> highlights), and `apply' scans the buffer as the function the macro
!> makes does.  A highlight's SYNTAX is a string (for `string_to_syntax'),
!> a procedure returning the property (upstream's expression), or either
!> of them with code run after the property is applied (upstream's
!> (prog1 SYNTAX . EXPS)); code alone is upstream's (ignore . EXPS).
!> Precompiled rule sets (`syntax_propertize_precompile_rules') are
!> arrays of rules, joined as Fortran arrays.
!>
!> Deferred: `syntax_propertize_via_font_lock' (font-lock-syntactic-keywords
!> are not ported), `syntax_ppss_debug' and `syntax_ppss_stats'.
module syntax_propertize_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_ARGS_OUT_OF_RANGE
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, cur, buffer_serial_live
    use lisp_fm, only: value_type, nil, nilp, list, symbolp, make_syntax_table_ref
    use lread_fm, only: sym
    use data_fm, only: value_eq, add_local_variables_killer
    use eval_fm, only: region_hook_type, region_entry_type
    use insdel_fm, only: before_change_functions
    use textprop_fm, only: get_text_property, put_text_property, remove_text_properties, &
                           previous_single_property_change, text_property_any
    use syntax_fm, only: parse_state_type, parse_partial_sexp, syntax_table, set_syntax_table, syntax_table_live_p, &
                         string_to_syntax, syntax_descriptor_type, syntax_descriptor_value, syntax_propertize__done, &
                         parse_sexp_lookup_properties, internal_syntax_propertize, syntax_ppss_function
    use search_fm, only: re_search_forward, string_match, match_beginning, match_end, match_data, set_match_data, &
                         string_index_to_offset, SEARCH_NOERROR
    use subr_fm, only: silent_modifications_type, silent_modifications_begin, silent_modifications_end, &
                       subregexp_context_p
    use editfns_fm, only: point, goto_char, point_min, point_max, bolp, line_beginning_position, &
                          inhibit_field_text_motion
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    use xdisp_fm, only: message
    use regexp_opt_fm, only: regexp_opt_depth
    implicit none(type, external)
    private

    public :: syntax_propertize, internal_syntax_propertize_function, syntax_ppss_cached, syntax_ppss_flush_cache, &
              syntax_propertize_wholelines, syntax_propertize_multiline, syntax_propertize_shift_groups_and_backrefs, &
              syntax_propertize_in_process_p, syntax_ppss_depth, syntax_ppss_toplevel_pos, syntax_ppss_context, &
              syms_of_syntax_propertize

    !> What `syntax_ppss_context' returns: neither, a string, a comment.
    integer, parameter, public :: SYNTAX_PPSS_NONE = 0, SYNTAX_PPSS_STRING = 1, SYNTAX_PPSS_COMMENT = 2

    !> most-positive-fixnum, the value of `syntax_propertize__done' while
    !> `syntax_propertize' runs (upstream's let-binding against recursion).
    integer(int64), parameter :: MOST_POSITIVE_FIXNUM = 2_int64**61 - 1

    !> A value of the `syntax_table' property: a raw syntax descriptor,
    !> or the syntax table `table' (when `is_table'); `apply' false is nil.
    type, public :: syntax_property_type
        logical :: apply = .false.
        type(syntax_descriptor_type) :: descriptor
        logical :: is_table = .false.
        type(handle_type) :: table
    end type syntax_property_type

    abstract interface
        !> A `syntax_propertize_function': apply `syntax_table' properties
        !> to the text from START to END.
        subroutine syntax_propertize_proc(start, end, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: start, end
            type(status_type), intent(out) :: status
        end subroutine syntax_propertize_proc

        !> The SYNTAX of a highlight given as code: the value to apply as
        !> `syntax_table' property, in PROPERTY (none for nil).  It may move
        !> point.
        subroutine syntax_value_proc(property, status)
            import :: syntax_property_type, status_type
            implicit none(type, external)
            type(syntax_property_type), intent(out) :: property
            type(status_type), intent(out) :: status
        end subroutine syntax_value_proc

        !> Code run by a highlight (the EXPS of (prog1 SYNTAX . EXPS) or
        !> (ignore . EXPS)).
        subroutine syntax_code_proc(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine syntax_code_proc

        !> `syntax_begin_function': move back outside of any
        !> comment/string/paren.
        subroutine syntax_begin_proc()
            implicit none(type, external)
        end subroutine syntax_begin_proc
    end interface

    !> One (NUMBER SYNTAX) of a rule: apply SYNTAX to the chars matched by
    !> subgroup NUMBER (`group') of the rule's regexp.  SYNTAX is `syntax'
    !> (a string, converted with `string_to_syntax') or `value' (code
    !> returning it); `after' is code run after the property is applied;
    !> with neither `syntax' nor `value', only `after' runs.
    type, public :: syntax_highlight_type
        integer :: group = 0
        character(len=:), allocatable :: syntax
        procedure(syntax_value_proc), pointer, nopass :: value => null()
        procedure(syntax_code_proc), pointer, nopass :: after => null()
    end type syntax_highlight_type

    !> A rule: (REGEXP HIGHLIGHT1 ... HIGHLIGHTn).
    type, public :: syntax_propertize_rule_type
        character(len=:), allocatable :: regexp
        type(syntax_highlight_type), allocatable :: highlights(:)
    end type syntax_propertize_rule_type

    ! One highlight as the code of a branch: the group GN after
    ! shifting, whether it runs only when GN matched.
    type :: rule_step_type
        integer :: gn = 0
        logical :: conditional = .false.
        type(syntax_highlight_type) :: highlight
        type(syntax_descriptor_type) :: descriptor
    end type rule_step_type

    ! One branch of the `cond' the macro builds.
    type :: rule_branch_type
        logical :: always = .false.
        integer, allocatable :: groups(:)
        integer :: shift = 0
        type(rule_step_type), allocatable :: steps(:)
    end type rule_branch_type

    !> What `syntax_propertize_rules' makes: RULES (assign them, then use
    !> `apply' as or in a `syntax_propertize_function').  The buffer is
    !> scanned a single time, like "lex" would, rather than once per rule.
    !> There may be at most nine back-references in the regexps of all
    !> rules in total.
    type, public :: syntax_propertize_rules_type
        type(syntax_propertize_rule_type), allocatable :: rules(:)
        logical, private :: compiled = .false.
        character(len=:), allocatable, private :: re
        type(rule_branch_type), allocatable, private :: branches(:)
    contains
        !> Apply the rules to the text from START to END.
        procedure :: apply => rules_apply
    end type syntax_propertize_rules_type

    ! One buffer's `syntax_propertize_function'.
    type :: function_entry_type
        integer(int64) :: serial = 0
        procedure(syntax_propertize_proc), pointer, nopass :: fn => null()
    end type function_entry_type

    !> The type of `syntax_propertize_function': a default `value'
    !> (assign with `=>'; null is nil) and buffers' own (`set_local').
    type, public :: syntax_propertize_function_type
        procedure(syntax_propertize_proc), pointer, nopass :: value => null()
        type(function_entry_type), allocatable :: entries(:)
    contains
        !> Make FN the current buffer's function (setq-local).
        procedure :: set_local => function_set_local
        !> The current buffer's function (null for nil).
        procedure :: get => function_get
        !> Make the current buffer use the default again.
        procedure :: kill_local => function_kill_local
    end type syntax_propertize_function_type

    ! A cache: LAST, a pair (LAST-POS . LAST-PPSS) caching the last
    ! invocation (`last_pos' 0 is nil), and CACHE, (POS . PPSS) pairs in
    ! decreasing POS order.
    type :: ppss_cell_type
        logical :: exists = .false.
        logical :: has_last = .false.
        integer(int64) :: last_pos = 0
        type(parse_state_type) :: last_ppss
        integer(int64), allocatable :: cache_pos(:)
        type(parse_state_type), allocatable :: cache_ppss(:)
    end type ppss_cell_type

    ! A buffer's own variables: `syntax_ppss_wide', `syntax_ppss_narrow',
    ! `syntax_ppss_narrow_start' (0 is nil), `syntax_ppss__updated_cache'
    ! and `syntax_ppss_table'.
    type :: ppss_data_type
        integer(int64) :: serial = 0
        type(ppss_cell_type) :: wide, narrow
        integer(int64) :: narrow_start = 0
        logical :: updated_cache = .false.
        logical :: has_table = .false.
        type(handle_type) :: table
    end type ppss_data_type

    !> The type of `syntax_ppss_table': buffers' own syntax tables
    !> (`set_local'; no table is nil).
    type, public :: syntax_ppss_table_type
    contains
        !> Make H the current buffer's table (setq-local).
        procedure, nopass :: set_local => ppss_table_set_local
        !> Whether the current buffer has a table, and which (FOUND).
        procedure, nopass :: get => ppss_table_get
        !> Make the current buffer's value nil again.
        procedure, nopass :: kill_local => ppss_table_kill_local
    end type syntax_ppss_table_type

    !> Mode-specific function to apply `syntax_table' text properties.
    !> It is the work horse of `syntax_propertize', which is called by things like
    !> Font-Lock and indentation.
    !>
    !> It is given two arguments, START and END: the start and end of the text to
    !> which `syntax_table' might need to be applied.  Major modes can use this to
    !> override the buffer's syntax table for special syntactic constructs that
    !> cannot be handled just by the buffer's syntax-table.
    !>
    !> The specified function may call `syntax_ppss' on any position
    !> before END, but if it calls `syntax_ppss' on some
    !> position and later modifies the buffer on some earlier position,
    !> then it is its responsibility to call `syntax_ppss_flush_cache' to flush
    !> the now obsolete ppss info from the cache.
    !>
    !> Note: When this variable is a function, it must apply _all_ the
    !> `syntax_table' properties needed in the given text interval.
    !> Using both this function and other means to apply these
    !> properties won't work properly.  Port of syntax.el
    !> syntax-propertize-function.
    type(syntax_propertize_function_type), public :: syntax_propertize_function

    !> How far `syntax_propertize' goes at least.  Port of syntax.el
    !> syntax-propertize-chunk-size.
    integer(int64), public :: syntax_propertize_chunk_size = 500

    !> Special hook run just before proceeding to propertize a region.
    !> This is used to allow major modes to help `syntax_propertize' find safe buffer
    !> positions as beginning and end of the propertized region.  Its most common use
    !> is to solve the problem of /identification/ of multiline elements by providing
    !> a function that tries to find such elements and move the boundaries such that
    !> they do not fall in the middle of one.
    !> Each function is called with two arguments (START and END) and it should return
    !> the new start and end (START and END themselves if no adjustment should be made).
    !> These functions are run in turn repeatedly until they all return no change.
    !> Put first the functions more likely to cause a change and cheaper to compute.
    !> Port of syntax.el syntax-propertize-extend-region-functions.
    type(region_hook_type), public, target :: syntax_propertize_extend_region_functions = &
                                              region_hook_type(name="syntax_propertize_extend_region_functions")

    !> Maximum line length for syntax operations.
    !> If lines are longer than that, syntax operations will treat them as chunks
    !> of this size.  Misfontification may then occur.
    !> This is a tradeoff between correctly applying the syntax rules,
    !> and avoiding major slowdown on pathologically long lines.  Port of
    !> syntax.el syntax-wholeline-max.
    integer(int64), public :: syntax_wholeline_max = 10000

    !> Syntax-table to use during `syntax_ppss', if any.  Port of
    !> syntax.el syntax-ppss-table.
    type(syntax_ppss_table_type), public :: syntax_ppss_table

    !> How far ahead of the position the syntax scanner needs
    !> `internal_syntax_propertize_function' propertizes.  Port of
    !> syntax.el syntax-propertize-chunks.
    integer(int64), public :: syntax_propertize_chunks = 2000

    !> Threshold below which cache info is deemed unnecessary.
    !> We try to make sure that cache entries are at least this far apart
    !> from each other, to avoid keeping too much useless info.  Port of
    !> syntax.el syntax-ppss-max-span.
    integer(int64), public :: syntax_ppss_max_span = 20000

    !> Function to move back outside of any comment/string/paren.
    !> This function should move the cursor back to some syntactically safe
    !> point (where the PPSS is equivalent to nil).  Port of syntax.el
    !> syntax-begin-function (obsolete since 25.1).
    procedure(syntax_begin_proc), pointer, public :: syntax_begin_function => null()

    ! Statistics about which case is more/less frequent in `syntax_ppss'
    ! (count . total distance); the 5th slot drives the heuristic to use
    ! `syntax_begin_function'.  syntax.el syntax-ppss-stats.
    integer(int64) :: stats_count(0:5) = [0_int64, 0_int64, 0_int64, 0_int64, 0_int64, 2_int64]
    integer(int64) :: stats_total(0:5) = [0_int64, 0_int64, 0_int64, 0_int64, 0_int64, 2500_int64]

    type(ppss_data_type), allocatable :: data(:)

contains

    ! ------------------------------------------------------------------
    ! Buffer-local values
    ! ------------------------------------------------------------------

    ! The index in `data' of the current buffer's variables (created).
    integer function data_index() result(k)
        type(ppss_data_type), allocatable :: grown(:)
        integer :: i, n

        if (.not. allocated(data)) allocate (data(0))

        do k = 1, size(data)
            if (data(k)%serial == buffers(cur)%serial) return
        end do

        do k = 1, size(data)
            if (.not. buffer_serial_live(data(k)%serial)) then
                data(k) = ppss_data_type(serial=buffers(cur)%serial)
                return
            end if
        end do

        n = size(data)
        allocate (grown(n + 1))

        do i = 1, n
            call move_cell(data(i)%wide, grown(i)%wide)
            call move_cell(data(i)%narrow, grown(i)%narrow)
            grown(i)%serial = data(i)%serial
            grown(i)%narrow_start = data(i)%narrow_start
            grown(i)%updated_cache = data(i)%updated_cache
            grown(i)%has_table = data(i)%has_table
            grown(i)%table = data(i)%table
        end do

        grown(n + 1)%serial = buffers(cur)%serial
        call move_alloc(grown, data)
        k = n + 1
    end function data_index

    subroutine move_cell(from, to)
        type(ppss_cell_type), intent(inout) :: from
        type(ppss_cell_type), intent(inout) :: to

        to%exists = from%exists
        to%has_last = from%has_last
        to%last_pos = from%last_pos
        to%last_ppss = from%last_ppss
        if (allocated(from%cache_pos)) call move_alloc(from%cache_pos, to%cache_pos)
        if (allocated(from%cache_ppss)) call move_alloc(from%cache_ppss, to%cache_ppss)
    end subroutine move_cell

    subroutine function_set_local(self, fn)
        class(syntax_propertize_function_type), intent(inout) :: self
        procedure(syntax_propertize_proc) :: fn
        type(function_entry_type), allocatable :: grown(:)
        integer :: i, n

        if (cur == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                self%entries(i)%fn => fn
                return
            end if
        end do

        do i = 1, size(self%entries)
            if (.not. buffer_serial_live(self%entries(i)%serial)) then
                self%entries(i)%serial = buffers(cur)%serial
                self%entries(i)%fn => fn
                return
            end if
        end do

        n = size(self%entries)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i)%serial = self%entries(i)%serial
            grown(i)%fn => self%entries(i)%fn
        end do

        grown(n + 1)%serial = buffers(cur)%serial
        grown(n + 1)%fn => fn
        call move_alloc(grown, self%entries)
    end subroutine function_set_local

    function function_get(self) result(fn)
        class(syntax_propertize_function_type), intent(in) :: self
        procedure(syntax_propertize_proc), pointer :: fn
        integer :: i

        fn => self%value
        if (cur == 0) return
        if (.not. allocated(self%entries)) return

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                if (associated(self%entries(i)%fn)) fn => self%entries(i)%fn
            end if
        end do
    end function function_get

    subroutine function_kill_local(self)
        class(syntax_propertize_function_type), intent(inout) :: self
        integer :: i

        if (cur == 0) return
        if (.not. allocated(self%entries)) return

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                self%entries(i)%serial = 0
                self%entries(i)%fn => null()
            end if
        end do
    end subroutine function_kill_local

    subroutine ppss_table_set_local(h)
        type(handle_type), intent(in) :: h
        integer :: k

        if (cur == 0) return
        k = data_index()
        data(k)%has_table = .true.
        data(k)%table = h
    end subroutine ppss_table_set_local

    subroutine ppss_table_get(h, found)
        type(handle_type), intent(out) :: h
        logical, intent(out) :: found
        integer :: k

        found = .false.
        if (cur == 0) return
        k = data_index()
        found = data(k)%has_table
        if (found) h = data(k)%table
    end subroutine ppss_table_get

    subroutine ppss_table_kill_local()
        integer :: k

        if (cur == 0) return
        k = data_index()
        data(k)%has_table = .false.
    end subroutine ppss_table_kill_local

    ! kill-all-local-variables: forget the current buffer's values.
    subroutine kill_syntax_locals()
        integer :: k

        call syntax_propertize_function%kill_local()
        if (cur == 0) return
        k = data_index()
        data(k) = ppss_data_type(serial=buffers(cur)%serial)
    end subroutine kill_syntax_locals

    ! The table `syntax_propertize' and `syntax_ppss' parse with:
    ! (or syntax-ppss-table (syntax-table)).
    function ppss_syntax_table() result(h)
        type(handle_type) :: h
        logical :: found

        call ppss_table_get(h, found)

        if (found) then
            if (syntax_table_live_p(h)) return
        end if

        h = syntax_table()
    end function ppss_syntax_table

    ! ------------------------------------------------------------------
    ! Applying syntax-table properties where needed
    ! ------------------------------------------------------------------

    !> Like `line_beginning_position' but obeying `syntax_wholeline_max'.
    !> Port of syntax.el:syntax--lbp.
    integer(int64) function syntax_lbp(arg) result(res)
        integer(int64), intent(in), optional :: arg
        integer(int64) :: pos, max

        pos = point()
        res = line_beginning_position(arg)
        if (abs(pos - res) < syntax_wholeline_max) return
        ! For lines that are too long, round to the nearest multiple of
        ! `syntax_wholeline_max'.  We use rounding rather than just
        ! (min res (+ pos syntax-wholeline-max)) so that repeated calls
        ! to `syntax_propertize_wholelines' don't keep growing the bounds,
        ! i.e. it really behaves like additional line-breaks.
        max = syntax_wholeline_max

        if (res < pos) then
            res = maxval([point_min(), max*(pos/max)])
        else
            res = minval([point_max(), max*((pos + max - 1)/max)])
        end if
    end function syntax_lbp

    !> Extend the region delimited by BEG and END to whole lines (in
    !> NEW_BEG and NEW_END).
    !> This function is useful for
    !> `syntax_propertize_extend_region_functions';
    !> see Info node `(elisp) Syntax Properties'.  Port of
    !> syntax.el:syntax-propertize-wholelines.
    subroutine syntax_propertize_wholelines(beg, end, new_beg, new_end, status)
        integer(int64), intent(in) :: beg, end
        integer(int64), intent(out) :: new_beg, new_end
        type(status_type), intent(out) :: status
        logical :: saved

        status = status_type()
        saved = inhibit_field_text_motion
        inhibit_field_text_motion = .true.
        call goto_char(beg)
        new_beg = beg
        if (.not. bolp()) new_beg = syntax_lbp()
        call goto_char(end)
        new_end = end
        if (.not. bolp()) new_end = syntax_lbp(2_int64)
        inhibit_field_text_motion = saved
    end subroutine syntax_propertize_wholelines

    !> Let `syntax_propertize' pay attention to the syntax_multiline
    !> property.  Port of syntax.el:syntax-propertize-multiline.
    subroutine syntax_propertize_multiline(beg, end, new_beg, new_end, status)
        integer(int64), intent(in) :: beg, end
        integer(int64), intent(out) :: new_beg, new_end
        type(status_type), intent(out) :: status
        type(value_type) :: val
        integer(int64) :: pos
        logical :: found

        new_beg = beg
        new_end = end
        status = status_type()

        if (beg > point_min()) then
            call get_text_property(beg - 1, sym("syntax_multiline"), val, status)
            if (.not. status%ok()) return

            if (.not. nilp(val)) then
                call previous_single_property_change(beg, sym("syntax_multiline"), pos, found, status)
                if (.not. status%ok()) return
                new_beg = point_min()
                if (found) new_beg = pos
            end if
        end if

        call get_text_property(end, sym("syntax_multiline"), val, status)
        if (.not. status%ok()) return

        if (.not. nilp(val)) then
            call text_property_any(end, point_max(), sym("syntax_multiline"), nil(), pos, found, status)
            if (.not. status%ok()) return
            new_end = point_max()
            if (found) new_end = pos
        end if
    end subroutine syntax_propertize_multiline

    !> RE with its explicitly numbered groups and its back-references
    !> shifted by N.  Port of
    !> syntax.el:syntax-propertize--shift-groups-and-backrefs.
    recursive function syntax_propertize_shift_groups_and_backrefs(re, n, status) result(new_re)
        character(len=*), intent(in) :: re
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: new_re
        integer(int64) :: pos, idx, mb, me, mb0
        integer :: num, ios
        logical :: context
        character(len=12) :: buf
        character(len=:), allocatable :: digits

        new_re = ""
        ! (replace-regexp-in-string "\\\\(\\?\\([0-9]+\\):" ... re t t)
        pos = 1

        do
            idx = string_match("\\(\?\([0-9]+\):", re, status, pos)
            if (.not. status%ok()) return
            if (idx == 0) exit
            mb = match_beginning(1, status)
            me = match_end(1, status)
            digits = chars(re, mb, me)
            read (digits, *, iostat=ios) num
            if (ios /= 0) num = 0
            write (buf, "(i0)") n + num
            new_re = new_re//chars(re, pos, mb)//trim(buf)
            pos = me
        end do

        new_re = new_re//chars(re, pos, char_count(re) + 1)
        pos = 1

        do
            idx = string_match("\\\([0-9]+\)", new_re, status, pos)
            if (.not. status%ok()) return
            if (idx == 0) exit
            mb0 = match_beginning(0, status)
            mb = match_beginning(1, status)
            me = match_end(1, status)
            pos = 1 + mb
            ! With \N, the \ must be in a subregexp context, i.e.,
            ! not in a character class or in a \{\} repetition.
            context = subregexp_context_p(new_re, mb0, status)
            if (.not. status%ok()) return

            if (context) then
                digits = chars(new_re, mb, me)
                read (digits, *, iostat=ios) num
                if (ios /= 0) num = 0
                num = n + num

                if (num > 9) then
                    call set_error(status, FM_GENERIC_ERROR, "There may be at most nine back-references")
                    return
                end if

                write (buf, "(i0)") num
                new_re = chars(new_re, 1_int64, mb)//trim(buf)//chars(new_re, me, char_count(new_re) + 1)
            end if
        end do
    end function syntax_propertize_shift_groups_and_backrefs

    ! The characters of S from index B (1-based) up to before index E.
    pure function chars(s, b, e) result(sub)
        character(len=*), intent(in) :: s
        integer(int64), intent(in) :: b, e
        character(len=:), allocatable :: sub

        sub = s(string_index_to_offset(s, b) + 1:string_index_to_offset(s, e))
    end function chars

    ! The number of characters of S.
    pure integer(int64) function char_count(s) result(n)
        character(len=*), intent(in) :: s
        integer :: i

        n = 0

        do i = 1, len(s)
            if (iand(ichar(s(i:i)), 192) /= 128) n = n + 1
        end do
    end function char_count

    ! Build the regexp and the branches of the rules, as
    ! syntax-propertize-rules does at macro-expansion time.
    recursive subroutine rules_compile(self, status)
        class(syntax_propertize_rules_type), intent(inout) :: self
        type(status_type), intent(out) :: status
        integer(int64), allocatable :: saved(:)
        character(len=:), allocatable :: re
        integer :: offset, local_offset, i, j, nrules, nh, depth
        logical :: has0, nocode

        status = status_type()
        saved = match_data()
        nrules = 0
        if (allocated(self%rules)) nrules = size(self%rules)
        if (allocated(self%branches)) deallocate (self%branches)
        allocate (self%branches(nrules))
        self%re = ""
        offset = 0

        do i = 1, nrules
            associate (rule => self%rules(i), branch => self%branches(i))
                nh = 0
                if (allocated(rule%highlights)) nh = size(rule%highlights)
                re = rule%regexp
                has0 = .false.

                do j = 1, nh
                    if (rule%highlights(j)%group == 0) has0 = .true.
                end do

                if (has0 .and. nrules > 1) then
                    ! If there's more than 1 rule, and the rule want to apply
                    ! highlight to match 0, create an extra group to be able to
                    ! tell when *this* match 0 has succeeded.
                    offset = offset + 1
                    re = "\("//re//"\)"
                end if

                re = syntax_propertize_shift_groups_and_backrefs(re, offset, status)
                if (.not. status%ok()) exit
                branch%always = .false.

                if (has0) then
                    if (offset == 0) then
                        branch%always = .true.
                        allocate (branch%groups(0))
                    else
                        branch%groups = [offset]
                    end if
                else
                    allocate (branch%groups(nh))

                    do j = 1, nh
                        branch%groups(j) = offset + rule%highlights(j)%group
                    end do
                end if

                local_offset = offset
                nocode = .true.
                branch%shift = 0

                ! If some of the subgroup rules include code, then we
                ! need to set the match-data so it's consistent with what the
                ! code expects.  If not, then we can simply use shifted
                ! offset in our own code.
                if (offset /= 0) then
                    do j = 1, nh
                        if (.not. allocated(rule%highlights(j)%syntax) .or. &
                            associated(rule%highlights(j)%after)) nocode = .false.
                    end do

                    if (.not. nocode) then
                        ! Keep match 0 as is, but shift everything else.
                        branch%shift = offset
                        local_offset = 0
                    end if
                end if

                ! Now construct the code for each subgroup rules.
                allocate (branch%steps(nh))

                do j = 1, nh
                    branch%steps(j)%gn = local_offset + rule%highlights(j)%group
                    branch%steps(j)%highlight = rule%highlights(j)
                    branch%steps(j)%conditional = nh > 1 .and. branch%steps(j)%gn /= 0

                    if (allocated(rule%highlights(j)%syntax)) then
                        call string_to_syntax(rule%highlights(j)%syntax, branch%steps(j)%descriptor, status)
                        if (.not. status%ok()) exit
                    end if
                end do

                if (.not. status%ok()) exit
                depth = regexp_opt_depth(rule%regexp, status)
                if (.not. status%ok()) exit
                offset = offset + depth
                if (i > 1) self%re = self%re//"\|"
                self%re = self%re//re
            end associate
        end do

        call set_match_data(saved)
        if (status%ok()) self%compiled = .true.
    end subroutine rules_compile

    !> Apply the rules from START to END: the function
    !> syntax.el:syntax-propertize-rules makes.
    recursive subroutine rules_apply(self, start, end, status)
        class(syntax_propertize_rules_type), intent(inout) :: self
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64) :: found
        integer :: i, j
        logical :: matched

        status = status_type()

        if (.not. self%compiled) then
            call rules_compile(self, status)
            if (.not. status%ok()) return
        end if

        call goto_char(start)

        do while (point() < end)
            call re_search_forward(self%re, found, status, bound=end, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return
            if (found == 0) exit

            do i = 1, size(self%branches)
                associate (branch => self%branches(i))
                    matched = branch%always

                    do j = 1, size(branch%groups)
                        if (matched) exit
                        matched = group_matched(branch%groups(j))
                    end do

                    if (.not. matched) cycle
                    if (branch%shift > 0) call shift_match_data(branch%shift)

                    do j = 1, size(branch%steps)
                        if (branch%steps(j)%conditional) then
                            if (.not. group_matched(branch%steps(j)%gn)) cycle
                        end if

                        call run_step(branch%steps(j), status)
                        if (.not. status%ok()) return
                    end do
                end associate

                exit
            end do
        end do
    end subroutine rules_apply

    ! Whether group N of the last match matched: (match-beginning N).
    logical function group_matched(n)
        integer, intent(in) :: n
        type(status_type) :: st

        group_matched = match_beginning(n, st) > 0
    end function group_matched

    ! Keep match 0 as is, but drop the first N groups.
    subroutine shift_match_data(n)
        integer, intent(in) :: n
        integer(int64), allocatable :: md(:), shifted(:)
        integer :: k

        md = match_data()
        k = 2*(n + 1)

        if (size(md) > k) then
            allocate (shifted(2 + size(md) - k))
            shifted(1:2) = md(1:2)
            shifted(3:) = md(k + 1:)
        else
            shifted = md(1:min(2, size(md)))
        end if

        call set_match_data(shifted)
    end subroutine shift_match_data

    ! Run one highlight of a matched rule.
    recursive subroutine run_step(step, status)
        type(rule_step_type), intent(in) :: step
        type(status_type), intent(out) :: status
        type(syntax_property_type) :: prop
        integer(int64) :: mb, me

        status = status_type()

        if (allocated(step%highlight%syntax)) then
            mb = match_beginning(step%gn, status)
            me = match_end(step%gn, status)
            call put_text_property(mb, me, sym("syntax_table"), syntax_descriptor_value(step%descriptor), status)
            if (.not. status%ok()) return
        else if (associated(step%highlight%value)) then
            mb = match_beginning(step%gn, status)
            me = match_end(step%gn, status)
            call step%highlight%value(prop, status)
            if (.not. status%ok()) return

            if (prop%apply) then
                call put_property(mb, me, prop, status)
                if (.not. status%ok()) return
            end if
        end if

        if (associated(step%highlight%after)) call step%highlight%after(status)
    end subroutine run_step

    ! Give START..END the `syntax_table' property PROP.
    subroutine put_property(start, end, prop, status)
        integer(int64), intent(in) :: start, end
        type(syntax_property_type), intent(in) :: prop
        type(status_type), intent(out) :: status

        if (prop%is_table) then
            call put_text_property(start, end, sym("syntax_table"), make_syntax_table_ref(prop%table), status)
        else
            call put_text_property(start, end, sym("syntax_table"), syntax_descriptor_value(prop%descriptor), status)
        end if
    end subroutine put_property

    !> True if we're inside `syntax_propertize'.
    !> This is used to avoid infinite recursion as well as to handle cases where
    !> `syntax_ppss' is called when the final `syntax_table' properties have not
    !> yet been setup, in which case we may end up putting invalid info into the cache.
    !> It's also used so that `syntax_ppss_flush_cache' can be used from within
    !> `syntax_propertize' without ruining the `syntax_table' already set.  Port
    !> of syntax.el:syntax-propertize--in-process-p.
    logical function syntax_propertize_in_process_p()
        syntax_propertize_in_process_p = syntax_propertize__done%value == MOST_POSITIVE_FIXNUM
    end function syntax_propertize_in_process_p

    !> Ensure that syntax-table properties are set until POS (a buffer point).
    !> Port of syntax.el:syntax-propertize.
    recursive subroutine syntax_propertize(pos, status)
        integer(int64), intent(in) :: pos
        type(status_type), intent(out) :: status
        procedure(syntax_propertize_proc), pointer :: fn
        type(region_entry_type), allocatable :: fns(:)
        type(silent_modifications_type) :: silent
        type(handle_type) :: here, old_table
        type(status_type) :: st
        integer(int64) :: start, end, new_beg, new_end, saved_done, p
        integer :: i, k
        logical :: first, repeat, saved_updated, changed
        character(len=24) :: b1, b2

        status = status_type()
        if (.not. (syntax_propertize__done%value < pos)) return
        fn => syntax_propertize_function%get()

        if (.not. associated(fn)) then
            call syntax_propertize__done%set_local(max(point_max(), pos))
            return
        end if

        call parse_sexp_lookup_properties%set_local(.true.)

        if (syntax_propertize__done%value < point_min()) then
            if (syntax_propertize__done%value > 0) then
                write (b1, "(i0)") syntax_propertize__done%value
                write (b2, "(i0)") point_min()
                call message("Cannot syntax-propertize "//trim(b1)//".."//trim(b2)//" because of narrowing!")
            end if

            ! *Usually* syntax-propertize is called via syntax-ppss which
            ! takes care of adding syntax-ppss-flush-cache to b-c-f, but this
            ! is not *always* the case, so since we share a single "flush" function
            ! between syntax-ppss and syntax-propertize, we also have to make
            ! sure the flush function is installed here (bug#29767).
            call before_change_functions%add(flush_cache_hook, depth=99, local=.true.)
        end if

        call copy_marker_at(point(), here, .false., st)
        call silent_modifications_begin(silent)
        old_table = syntax_table()
        call set_syntax_table(ppss_syntax_table(), st)
        ! Just in case!
        call syntax_propertize__done%make_local()
        ! Make sure we let-bind it only buffer-locally.
        k = data_index()
        saved_updated = data(k)%updated_cache
        data(k)%updated_cache = .false.
        start = max(min(syntax_propertize__done%value, point_max()), point_min())
        end = max(pos, min(point_max(), start + syntax_propertize_chunk_size))
        first = .true.
        repeat = .true.

        body: block
            do while (repeat)
                repeat = .false.
                fns = syntax_propertize_extend_region_functions%functions()

                do i = 1, size(fns)
                    ! Bind `syntax_propertize__done' to avoid recursion!
                    saved_done = syntax_propertize__done%value
                    call syntax_propertize__done%set_local(MOST_POSITIVE_FIXNUM)
                    call fns(i)%fn(start, end, new_beg, new_end, status)
                    call syntax_propertize__done%set_local(saved_done)
                    if (.not. status%ok()) exit body
                    changed = new_beg /= start .or. new_end /= end

                    if (changed .and. .not. (new_beg >= start .and. new_end <= end)) then
                        start = new_beg
                        end = new_end
                        ! If there's been a change, we should go through the
                        ! list again since this new position may
                        ! warrant a different answer from one of the funs we've
                        ! already seen.
                        if (.not. first) repeat = .true.
                    end if

                    first = .false.
                end do
            end do

            ! Flush ppss cache between the original value of `start' and that
            ! set above by syntax-propertize-extend-region-functions.
            call syntax_ppss_flush_cache(start)
            ! Move the limit before calling the function, so it's
            ! done in case of errors.
            call syntax_propertize__done%set_local(end)
            call remove_text_properties(start, end, list(sym("syntax_table"), nil(), sym("syntax_multiline"), nil()), &
                                        changed, status)
            if (.not. status%ok()) exit body
            ! Bind `syntax_propertize__done' to avoid recursion!
            saved_done = syntax_propertize__done%value
            call syntax_propertize__done%set_local(MOST_POSITIVE_FIXNUM)
            call fn(start, end, status)

            if (status%ok()) then
                k = data_index()
                ! `syntax_ppss' was called and updated the cache while we
                ! were propertizing so we need to flush the part of the
                ! cache that may have been rendered out-of-date by the new
                ! properties.
                if (data(k)%updated_cache) call syntax_ppss_flush_cache(start)
            end if

            call syntax_propertize__done%set_local(saved_done)
        end block body

        k = data_index()
        data(k)%updated_cache = saved_updated
        call set_syntax_table(old_table, st)
        call silent_modifications_end(silent)
        call marker_position(here, p, st)
        call goto_char(p)
        call free_marker(here, st)
    end subroutine syntax_propertize

    !> Propertize far enough beyond CHARPOS for the syntax scanner (called
    !> from syntax_fm's parse_sexp_propertize; its errors are ignored, as
    !> upstream's safe_calln does).  Port of
    !> syntax.el:internal--syntax-propertize.
    recursive subroutine internal_syntax_propertize_function(charpos)
        integer(int64), intent(in) :: charpos
        integer(int64), allocatable :: saved(:)
        type(status_type) :: status

        ! save-match-data
        saved = match_data()
        call syntax_propertize(min(syntax_propertize_chunks + charpos, point_max()), status)
        call set_match_data(saved)
    end subroutine internal_syntax_propertize_function

    ! ------------------------------------------------------------------
    ! Incrementally compute and memoize parser state
    ! ------------------------------------------------------------------

    !> The depth in parens of PPSS.  Port of syntax.el:syntax-ppss-depth.
    integer(int64) function syntax_ppss_depth(ppss)
        type(parse_state_type), intent(in) :: ppss

        syntax_ppss_depth = ppss%depth
    end function syntax_ppss_depth

    !> Get the latest syntactically outermost position found in a syntactic scan.
    !> PPSS is a scan state, as returned by `parse_partial_sexp' or `syntax_ppss'.
    !> An "outermost position" means one that it is outside of any syntactic entity:
    !> outside of any parentheses, comments, or strings encountered in the scan.
    !> If no such position is recorded in PPSS (because the end of the scan was
    !> itself at the outermost level), return 0.  Port of
    !> syntax.el:syntax-ppss-toplevel-pos.
    integer(int64) function syntax_ppss_toplevel_pos(ppss) result(pos)
        type(parse_state_type), intent(in) :: ppss

        pos = 0

        if (allocated(ppss%open_parens)) then
            if (size(ppss%open_parens) > 0) then
                pos = ppss%open_parens(1)
                return
            end if
        end if

        pos = ppss%comstr_start
    end function syntax_ppss_toplevel_pos

    !> Say whether PPSS is a string, a comment, or something else.
    !> If PPSS is a string, `SYNTAX_PPSS_STRING' is returned.  If it's a
    !> comment, `SYNTAX_PPSS_COMMENT' is returned.  If it's something
    !> else, `SYNTAX_PPSS_NONE' is returned.  Port of
    !> syntax.el:syntax-ppss-context.
    integer function syntax_ppss_context(ppss) result(context)
        type(parse_state_type), intent(in) :: ppss

        context = SYNTAX_PPSS_NONE

        if (ppss%instring /= -1) then
            context = SYNTAX_PPSS_STRING
        else if (ppss%incomment /= 0) then
            context = SYNTAX_PPSS_COMMENT
        end if
    end function syntax_ppss_context

    ! `syntax_ppss_flush_cache' as a `before_change_functions' function.
    recursive subroutine flush_cache_hook(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status

        status = status_type()
        call syntax_ppss_flush_cache(beg)
        if (end < beg) continue
    end subroutine flush_cache_hook

    !> Flush the cache of `syntax_ppss' starting at position BEG.  Port of
    !> syntax.el:syntax-ppss-flush-cache.
    recursive subroutine syntax_ppss_flush_cache(beg)
        integer(int64), intent(in) :: beg
        integer :: k

        if (cur == 0) return

        ! Set syntax-propertize to refontify anything past beg.
        if (.not. syntax_propertize_in_process_p()) then
            call syntax_propertize__done%set_local(min(beg, syntax_propertize__done%value))
        end if

        ! Flush invalid cache entries.
        k = data_index()
        call flush_cell(data(k)%wide)
        call flush_cell(data(k)%narrow)

    contains

        subroutine flush_cell(cell)
            type(ppss_cell_type), intent(inout) :: cell
            integer :: n, drop
            integer(int64) :: limit

            if (.not. cell%exists) return
            n = 0
            if (allocated(cell%cache_pos)) n = size(cell%cache_pos)
            drop = 0

            do while (drop < n)
                if (cell%cache_pos(drop + 1) <= beg) exit
                drop = drop + 1
            end do

            if (drop > 0) call remove_entries(cell, drop)

            ! Throw away `last' value if made invalid.
            if (cell%has_last) then
                if (beg < cell%last_pos) then
                    ! If syntax-begin-function jumped to BEG, then the old state at BEG can
                    ! depend on the text after BEG (which is presumably changed).  So if
                    ! BEG=(car (nth 10 syntax-ppss-last)) don't reuse that data because the
                    ! assumed nil state at BEG may not be valid any more.
                    limit = syntax_ppss_toplevel_pos(cell%last_ppss)
                    if (limit == 0) limit = cell%last_ppss%last_sexp_start

                    if (beg <= limit) then
                        cell%has_last = .false.
                    else
                        cell%last_pos = 0
                    end if
                end if
            end if
        end subroutine flush_cell
    end subroutine syntax_ppss_flush_cache

    ! Drop the first N entries of CELL's cache.
    subroutine remove_entries(cell, n)
        type(ppss_cell_type), intent(inout) :: cell
        integer, intent(in) :: n
        integer(int64), allocatable :: pos(:)
        type(parse_state_type), allocatable :: ppss(:)
        integer :: i, m

        m = size(cell%cache_pos) - n
        allocate (pos(m), ppss(m))

        do i = 1, m
            pos(i) = cell%cache_pos(n + i)
            ppss(i) = cell%cache_ppss(n + i)
        end do

        call move_alloc(pos, cell%cache_pos)
        call move_alloc(ppss, cell%cache_ppss)
    end subroutine remove_entries

    ! Insert (POS . PPSS) into CELL's cache before entry AT.
    subroutine insert_entry(cell, at, pos, ppss)
        type(ppss_cell_type), intent(inout) :: cell
        integer, intent(in) :: at
        integer(int64), intent(in) :: pos
        type(parse_state_type), intent(in) :: ppss
        integer(int64), allocatable :: new_pos(:)
        type(parse_state_type), allocatable :: new_ppss(:)
        integer :: i, n

        n = 0
        if (allocated(cell%cache_pos)) n = size(cell%cache_pos)
        allocate (new_pos(n + 1), new_ppss(n + 1))

        do i = 1, at - 1
            new_pos(i) = cell%cache_pos(i)
            new_ppss(i) = cell%cache_ppss(i)
        end do

        new_pos(at) = pos
        new_ppss(at) = ppss

        do i = at, n
            new_pos(i + 1) = cell%cache_pos(i)
            new_ppss(i + 1) = cell%cache_ppss(i)
        end do

        call move_alloc(new_pos, cell%cache_pos)
        call move_alloc(new_ppss, cell%cache_ppss)
    end subroutine insert_entry

    subroutine update_stats(i, old, new)
        integer, intent(in) :: i
        integer(int64), intent(in) :: old, new

        stats_count(i) = stats_count(i) + 1
        stats_total(i) = stats_total(i) + (new - old)
    end subroutine update_stats

    ! The cell of the current buffer's restriction (syntax-ppss--data),
    ! in `data(k)'.
    subroutine ppss_data(k, wide)
        integer, intent(out) :: k
        logical, intent(out) :: wide

        k = data_index()
        wide = point_min() == 1

        if (wide) then
            data(k)%wide%exists = .true.
        else
            if (data(k)%narrow_start /= point_min()) then
                data(k)%narrow_start = point_min()
                data(k)%narrow = ppss_cell_type(exists=.true.)
            end if
        end if
    end subroutine ppss_data

    !> Parse-Partial-Sexp State at POS, defaulting to point.
    !> If POS is given, this function moves point to POS.
    !>
    !> The returned value is the same as that of `parse_partial_sexp'
    !> run from `point_min' to POS except that values at positions 2 and 6
    !> in the returned list (counting from 0) cannot be relied upon.
    !>
    !> It is necessary to call `syntax_ppss_flush_cache' explicitly if
    !> this function is called while `before_change_functions' is
    !> temporarily let-bound, or if the buffer is modified without
    !> running the hook.  Port of syntax.el:syntax-ppss.
    recursive subroutine syntax_ppss_cached(state, pos)
        type(parse_state_type), intent(out) :: state
        integer(int64), intent(in), optional :: pos
        type(ppss_cell_type) :: cell
        type(parse_state_type) :: old_ppss, ppss, ppss_best
        type(handle_type) :: old_table
        type(status_type) :: status, st
        type(value_type) :: face
        integer(int64) :: p, old_pos, pt_min, pt_best, new_min
        integer :: k, idx, cache_pred, n
        logical :: wide, has_old_ppss, has_ppss, has_best, use_begin

        ! Default values.
        p = point()
        if (present(pos)) p = pos
        call syntax_propertize(p, st)
        old_table = syntax_table()
        call set_syntax_table(ppss_syntax_table(), st)
        call ppss_data(k, wide)

        if (wide) then
            cell = data(k)%wide
        else
            cell = data(k)%narrow
        end if

        has_old_ppss = cell%has_last
        if (has_old_ppss) old_ppss = cell%last_ppss
        old_pos = 0
        if (cell%has_last) old_pos = cell%last_pos
        if (old_pos /= 0 .and. old_pos > p) old_pos = 0
        n = 0
        if (allocated(cell%cache_pos)) n = size(cell%cache_pos)

        body: block
            ! Use the OLD-POS if usable and close.  Don't update the `last' cache.
            if (old_pos /= 0) then
                if (p - old_pos < (2*stats_total(5))/stats_count(5)) then
                    call update_stats(0, old_pos, p)
                    call parse_partial_sexp(old_pos, p, state, status, oldstate=old_ppss)
                    if (.not. status%ok()) exit body
                    call set_syntax_table(old_table, st)
                    return
                end if
            end if

            ! Use OLD-PPSS if possible and close enough.
            pt_min = 0

            if (old_pos == 0 .and. has_old_ppss) then
                ! If `pt-min' is too far from `pos', we could try to use
                ! other positions in (nth 9 old-ppss), but that doesn't
                ! seem to happen in practice and it would complicate this
                ! code (and the before-change-function code even more).
                ! But maybe it would be useful in "degenerate" cases such
                ! as when the whole file is wrapped in a set
                ! of parentheses.
                pt_min = syntax_ppss_toplevel_pos(old_ppss)
                if (pt_min == 0) pt_min = old_ppss%last_sexp_start
            end if

            if (pt_min /= 0 .and. pt_min <= p .and. p - pt_min < syntax_ppss_max_span) then
                call update_stats(1, pt_min, p)
                call parse_partial_sexp(pt_min, p, ppss, status)
                if (.not. status%ok()) exit body
            else
                ! The OLD-* data can't be used.  Consult the cache.
                cache_pred = 0
                pt_min = point_min()
                ! I differentiate between PT-MIN and PT-BEST because
                ! I feel like it might be important to ensure that the
                ! cache is only filled with 100% sure data (whereas
                ! syntax-begin-function might return incorrect data).
                ! Maybe that's just stupid.
                pt_best = point_min()
                has_best = .false.
                has_ppss = .false.
                ! look for a usable cache entry.
                idx = 1

                do while (idx <= n)
                    if (.not. p < cell%cache_pos(idx)) exit
                    cache_pred = idx
                    idx = idx + 1
                end do

                if (idx <= n) then
                    pt_min = cell%cache_pos(idx)
                    ppss = cell%cache_ppss(idx)
                    has_ppss = .true.
                end if

                ! Setup the before-change function if necessary.
                if (n == 0 .and. .not. cell%has_last) then
                    ! Note: combine-change-calls-1 needs to be kept in sync
                    ! with this!
                    ! We should be either the very last function on
                    ! before-change-functions or the very first on
                    ! after-change-functions.
                    call before_change_functions%add(flush_cache_hook, depth=99, local=.true.)
                end if

                ! Use the best of OLD-POS and CACHE.
                if (old_pos == 0 .or. old_pos < pt_min) then
                    pt_best = pt_min
                    has_best = has_ppss
                    if (has_ppss) ppss_best = ppss
                else
                    call update_stats(4, old_pos, p)
                    pt_best = old_pos
                    has_best = has_old_ppss
                    if (has_best) ppss_best = old_ppss
                end if

                ! Use the `syntax-begin-function' if available.
                ! We could try using that function earlier, but:
                ! - The result might not be 100% reliable, so it's better to use
                !   the cache if available.
                ! - The function might be slow.
                ! - If this function almost always finds a safe nearby spot,
                !   the cache won't be populated, so consulting it is cheap.
                if (associated(syntax_begin_function)) then
                    call goto_char(p)
                    call syntax_begin_function()
                    ! Make sure it's better.
                    use_begin = point() > pt_best
                    ! Simple sanity checks.
                    ! backward-paragraph can fail here.
                    if (use_begin) use_begin = point() < p

                    if (use_begin) then
                        call get_text_property(point(), sym("face"), face, st)

                        if (symbolp(face)) then
                            if (value_eq(face, sym("font_lock_string_face")) .or. &
                                value_eq(face, sym("font_lock_doc_face")) .or. &
                                value_eq(face, sym("font_lock_comment_face"))) use_begin = .false.
                        end if
                    end if

                    if (use_begin) then
                        call update_stats(5, point(), p)
                        pt_best = point()
                        has_best = .false.
                    end if
                end if

                if (p - pt_best < syntax_ppss_max_span) then
                    ! Quick case when we found a nearby pos.
                    call update_stats(2, pt_best, p)

                    if (has_best) then
                        call parse_partial_sexp(pt_best, p, ppss, status, oldstate=ppss_best)
                    else
                        call parse_partial_sexp(pt_best, p, ppss, status)
                    end if

                    if (.not. status%ok()) exit body
                else
                    ! Slow case: compute the state from some known position and
                    ! populate the cache so we won't need to do it again soon.
                    call update_stats(3, pt_min, p)
                    k = data_index()
                    data(k)%updated_cache = .true.

                    ! If `pt-min' is too far, add a few intermediate entries.
                    do while (p - pt_min > 2*syntax_ppss_max_span)
                        new_min = (pt_min + p)/2
                        call parse_ppss(pt_min, new_min)
                        if (.not. status%ok()) exit body
                        pt_min = new_min
                        call insert_entry(cell, cache_pred + 1, pt_min, ppss)
                    end do

                    ! Compute the actual return value.
                    call parse_ppss(pt_min, p)
                    if (.not. status%ok()) exit body
                    ! Store it in the cache.
                    n = 0
                    if (allocated(cell%cache_pos)) n = size(cell%cache_pos)

                    if (cache_pred > 0) then
                        if (cell%cache_pos(cache_pred) - p > syntax_ppss_max_span) then
                            call insert_entry(cell, cache_pred + 1, p, ppss)
                        else
                            cell%cache_pos(cache_pred) = p
                            cell%cache_ppss(cache_pred) = ppss
                        end if
                    else
                        if (n == 0) then
                            call insert_entry(cell, 1, p, ppss)
                        else if (cell%cache_pos(1) - p > syntax_ppss_max_span) then
                            call insert_entry(cell, 1, p, ppss)
                        else
                            cell%cache_pos(1) = p
                            cell%cache_ppss(1) = ppss
                        end if
                    end if
                end if
            end if

            k = data_index()
            data(k)%updated_cache = .true.
            cell%has_last = .true.
            cell%last_pos = p
            cell%last_ppss = ppss
            cell%exists = .true.

            if (wide) then
                data(k)%wide = cell
            else
                data(k)%narrow = cell
            end if

            state = ppss
            call set_syntax_table(old_table, st)
            return
        end block body

        ! If the buffer is more narrowed than when we built the cache,
        ! we may end up calling parse-partial-sexp with a position before
        ! point-min.  In that case, just parse from point-min assuming
        ! a nil state.
        if (status%code == FM_ARGS_OUT_OF_RANGE) call parse_partial_sexp(point_min(), p, state, status)
        call set_syntax_table(old_table, st)

    contains

        ! (parse-partial-sexp FROM TO nil nil ppss) into ppss.
        recursive subroutine parse_ppss(from, to)
            integer(int64), intent(in) :: from, to
            type(parse_state_type) :: next

            if (has_ppss) then
                call parse_partial_sexp(from, to, next, status, oldstate=ppss)
            else
                call parse_partial_sexp(from, to, next, status)
            end if

            ppss = next
            has_ppss = .true.
        end subroutine parse_ppss
    end subroutine syntax_ppss_cached

    !> Give the variables their defaults and hook syntax-propertize and
    !> the cache into syntax_fm.
    subroutine syms_of_syntax_propertize()
        call syntax_propertize_extend_region_functions%add(syntax_propertize_wholelines)
        call add_local_variables_killer(kill_syntax_locals)
        internal_syntax_propertize => internal_syntax_propertize_function
        syntax_ppss_function => syntax_ppss_cached
    end subroutine syms_of_syntax_propertize
end module syntax_propertize_fm
