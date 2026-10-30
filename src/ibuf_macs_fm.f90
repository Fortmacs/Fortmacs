! ibuf_macs_fm.f90 -- the types that define Ibuffer columns, sorting
! methods and operations.
! Ported from GNU Emacs 31.1 lisp/ibuf-macs.el.
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

!> Upstream's ibuf-macs.el defines macros that define Ibuffer's columns
!> (`define-ibuffer-column'), sorting methods (`define-ibuffer-sorter'),
!> operations on marked buffers (`define-ibuffer-op') and filters
!> (`define-ibuffer-filter').  Here they are typed records: a column is
!> an `ibuffer_column_type' added to `ibuffer_columns' (ibuffer_fm), a
!> sorting method an `ibuffer_sorter_type' added to
!> `ibuffer_sorting_functions', an operation an `ibuffer_op_type' run by
!> ibuffer_fm's `ibuffer_operate'.  The entries of `ibuffer_formats' are
!> `ibuffer_column_spec_type' records, made with `ibuffer_column' and
!> `ibuffer_text':
!>
!>     call ibuffer_formats%clear()
!>     call ibuffer_formats%add([ibuffer_column("mark"), ibuffer_text(" "), &
!>                               ibuffer_column("name", 16, -1)])
!>
!> Deferred: `define-ibuffer-filter' (with the filters, step 36c),
!> `ibuffer-aif' and `ibuffer-awhen' (Lisp control macros: Fortran needs
!> none), INLINE columns (a byte-compiler optimization), the
!> HEADER-MOUSE-MAP of a column, and an operation's commands for
!> user-defined sorting methods and operations (commands exist at compile
!> time; a sorting method added at run time is reached with
!> \[ibuffer_toggle_sorting_mode]).
module ibuf_macs_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    implicit none(type, external)
    private

    public :: ibuffer_column, ibuffer_text, ibuffer_and, ibuffer_or, ibuffer_not, ibuffer_saved, ibuffer_filter, &
              ibuffer_major_mode, ibuffer_used_mode, ibuffer_derived_mode, ibuffer_name, ibuffer_process, &
              ibuffer_starred_name, ibuffer_filename, ibuffer_basename, ibuffer_file_extension, ibuffer_directory, &
              ibuffer_size_gt, ibuffer_size_lt, ibuffer_modified, ibuffer_visiting_file, ibuffer_content, &
              ibuffer_predicate, ibuffer_filter_extent, ibuffer_filter_part, ibuffer_filter_equal

    !> The ALIGN of an `ibuffer_column_spec_type' (:left, :center,
    !> :right).
    integer, parameter, public :: IBUFFER_ALIGN_LEFT = 0, IBUFFER_ALIGN_CENTER = 1, IBUFFER_ALIGN_RIGHT = 2

    !> What an operation's body returns for a buffer: it chose not to
    !> affect the buffer (nil), it did (t), or the buffer's line should be
    !> removed from the list (`kill').
    integer, parameter, public :: IBUFFER_OP_NIL = 0, IBUFFER_OP_T = 1, IBUFFER_OP_KILL = 2

    !> The MODIFIER_P of an operation: it never modifies buffers (nil), it
    !> always does (t), or compare the buffers' modification flags
    !> (:maybe).
    integer, parameter, public :: IBUFFER_MODIFIER_NIL = 0, IBUFFER_MODIFIER_T = 1, IBUFFER_MODIFIER_MAYBE = 2

    !> The longest name of a column, a sorting method or a mode, and of a
    !> literal string in a format.
    integer, parameter, public :: IBUFFER_NAME_LEN = 48, IBUFFER_TEXT_LEN = 64

    !> The text of a column for one buffer: TEXT, in FACE if that is not
    !> empty (a `font-lock-face' property); SIZE is the buffer size the
    !> size column shows (its `ibuffer-size' property; -1 for none) and
    !> PROCESS whether the text shows a process (`ibuffer-process').
    !> MOUSE_FACE and NAME_COLUMN are the column's other properties (the
    !> `mouse-face' and the `ibuffer-name-column' of the name column).
    type, public :: ibuffer_cell_type
        character(len=:), allocatable :: text
        character(len=:), allocatable :: face
        character(len=:), allocatable :: mouse_face
        integer(int64) :: size = -1
        logical :: process = .false.
        logical :: name_column = .false.
    end type ibuffer_cell_type

    abstract interface
        !> A size limit of a column computed when the list is drawn (a
        !> function of no arguments returning an integer).
        integer function ibuffer_size_proc()
            implicit none(type, external)
        end function ibuffer_size_proc

        !> A column's BODY: the text of the column for BUFFER, whose mark is
        !> MARK, made with BUFFER current (`define-ibuffer-column').
        recursive function ibuffer_column_proc(buffer, mark) result(cell)
            import :: handle_type, ibuffer_cell_type
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            character, intent(in) :: mark
            type(ibuffer_cell_type) :: cell
        end function ibuffer_column_proc

        !> A column's SUMMARIZER: the TEXT to display at the bottom of the
        !> column, from the column's CELLS (a subroutine: gfortran frees a
        !> pointer component to a function with a deferred-length result).
        subroutine ibuffer_summarizer_proc(cells, text)
            import :: ibuffer_cell_type
            implicit none(type, external)
            type(ibuffer_cell_type), intent(in) :: cells(:)
            character(len=:), allocatable, intent(out) :: text
        end subroutine ibuffer_summarizer_proc

        !> A sorting method: whether buffer A is "less than" buffer B
        !> (`define-ibuffer-sorter').
        recursive logical function ibuffer_sorter_proc(a, b)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), intent(in) :: a, b
        end function ibuffer_sorter_proc

        !> An operation's BODY for BUF, marked with MARK: RESULT is an
        !> IBUFFER_OP_* value.  Unless the operation is COMPLEX, BUF is
        !> current and point is saved around it.
        recursive subroutine ibuffer_op_proc(buf, mark, result, status)
            import :: handle_type, status_type
            implicit none(type, external)
            type(handle_type), intent(in) :: buf
            character, intent(in) :: mark
            integer, intent(out) :: result
            type(status_type), intent(out) :: status
        end subroutine ibuffer_op_proc

        !> An operation's BEFORE or AFTER form.
        recursive subroutine ibuffer_op_hook_proc(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine ibuffer_op_hook_proc

        !> A predicate on BUFFER (`ibuffer_maybe_show_predicates' and the
        !> like).
        recursive logical function ibuffer_buffer_predicate_proc(buffer)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
        end function ibuffer_buffer_predicate_proc

        !> The FORM of an `ibuffer_fontification_alist' entry, evaluated in
        !> the buffer.
        recursive logical function ibuffer_form_proc()
            implicit none(type, external)
        end function ibuffer_form_proc
    end interface

    public :: ibuffer_size_proc, ibuffer_column_proc, ibuffer_summarizer_proc, ibuffer_sorter_proc, ibuffer_op_proc, &
              ibuffer_op_hook_proc, ibuffer_buffer_predicate_proc, ibuffer_form_proc

    !> An entry of a format in `ibuffer_formats': a column NAME (such as
    !> "mark", "name" or "size"), or a literal string TEXT (its first
    !> TEXT_LEN characters) when NAME is blank.  MIN_SIZE and MAX_SIZE
    !> are integers (or MIN_FUNCTION and MAX_FUNCTION, functions of no
    !> arguments returning an integer) which constrict the size of a
    !> column.  If MAX_SIZE is -1, there is no upper bound.  If MIN_SIZE
    !> is negative, use the end of the string.  ALIGN describes the
    !> alignment of the column (IBUFFER_ALIGN_LEFT, _CENTER or _RIGHT);
    !> ELIDE describes whether or not to elide the column if it is too
    !> long.  A COLUMN or (SYMBOL MIN-SIZE MAX-SIZE &optional ALIGN ELIDE)
    !> or "STRING" upstream.
    type, public :: ibuffer_column_spec_type
        character(len=IBUFFER_NAME_LEN) :: name = ""
        character(len=IBUFFER_TEXT_LEN) :: text = ""
        integer :: text_len = 0
        integer :: min_size = 0
        integer :: max_size = -1
        procedure(ibuffer_size_proc), pointer, nopass :: min_function => null()
        procedure(ibuffer_size_proc), pointer, nopass :: max_function => null()
        integer :: align = IBUFFER_ALIGN_LEFT
        logical :: elide = .false.
    end type ibuffer_column_spec_type

    !> One way to display buffer lines: its entries in order.
    type, public :: ibuffer_format_type
        type(ibuffer_column_spec_type), allocatable :: entries(:)
    end type ibuffer_format_type

    !> A list of ways to display buffer lines (the type of
    !> `ibuffer_formats').
    type, public :: ibuffer_formats_type
        type(ibuffer_format_type), allocatable :: formats(:)
    contains
        !> Add a format: its ENTRIES, made with `ibuffer_column' and
        !> `ibuffer_text'.
        procedure :: add => formats_add
        !> Remove all the formats.
        procedure :: clear => formats_clear
        !> The number of formats.
        procedure :: count => formats_count
    end type ibuffer_formats_type

    !> A column for use with `ibuffer_formats': its NAME, its TITLE (" "
    !> for the mark column; the NAME capitalized by default), its BODY
    !> (MAKE), and a SUMMARIZER, given the texts of the column, to
    !> display at the bottom.  Port of ibuf-macs.el:define-ibuffer-column.
    type, public :: ibuffer_column_type
        character(len=IBUFFER_NAME_LEN) :: name = ""
        character(len=IBUFFER_NAME_LEN) :: title = ""
        procedure(ibuffer_column_proc), pointer, nopass :: make => null()
        procedure(ibuffer_summarizer_proc), pointer, nopass :: summarizer => null()
    end type ibuffer_column_type

    !> A method of sorting: its NAME (`ibuffer_sorting_mode'), a short
    !> DESCRIPTION, and the predicate LESS.  Port of
    !> ibuf-macs.el:define-ibuffer-sorter (an element of
    !> `ibuffer-sorting-functions-alist').
    type, public :: ibuffer_sorter_type
        character(len=IBUFFER_NAME_LEN) :: name = ""
        character(len=IBUFFER_NAME_LEN) :: description = ""
        procedure(ibuffer_sorter_proc), pointer, nopass :: less => null()
    end type ibuffer_sorter_type

    !> An operation on marked buffers.  DELETION true means it operates on
    !> buffers marked for deletion, otherwise on normally marked buffers
    !> (MARK).  MODIFIER_P describes how it modifies buffers (an
    !> IBUFFER_MODIFIER_* value).  DANGEROUS means the user is prompted
    !> before performing it, with "Really ACTIVE_OPSTRING x buffers?";
    !> OPSTRING is displayed after it completes ("Operation finished;
    !> OPSTRING x buffers").  BEFORE and AFTER run before and after it.
    !> COMPLEX means BODY runs without the buffer current.  Port of
    !> ibuf-macs.el:define-ibuffer-op.
    type, public :: ibuffer_op_type
        logical :: deletion = .false.
        integer :: modifier_p = IBUFFER_MODIFIER_NIL
        logical :: dangerous = .false.
        character(len=IBUFFER_TEXT_LEN) :: opstring = "operated on"
        character(len=IBUFFER_TEXT_LEN) :: active_opstring = "Operate on"
        logical :: complex = .false.
        procedure(ibuffer_op_hook_proc), pointer, nopass :: before => null()
        procedure(ibuffer_op_hook_proc), pointer, nopass :: after => null()
        procedure(ibuffer_op_proc), pointer, nopass :: body => null()
    end type ibuffer_op_type

    !> A predicate of `ibuffer_maybe_show_predicates',
    !> `ibuffer_never_show_predicates' or `ibuffer_always_show_predicates':
    !> a REGEXP matched against the buffer's name, or (when REGEXP is
    !> blank) a PREDICATE called with the buffer.
    type, public :: ibuffer_show_predicate_type
        character(len=256) :: regexp = ""
        procedure(ibuffer_buffer_predicate_proc), pointer, nopass :: predicate => null()
    end type ibuffer_show_predicate_type

    !> An element of `ibuffer_fontification_alist': PRIORITY, a FORM
    !> evaluated in the buffer, and the FACE to put on the buffer name if
    !> FORM returns true.
    type, public :: ibuffer_fontification_type
        integer :: priority = 0
        procedure(ibuffer_form_proc), pointer, nopass :: form => null()
        character(len=IBUFFER_NAME_LEN) :: face = ""
    end type ibuffer_fontification_type

    !> An element of `ibuffer_directory_abbrev_alist' (FROM . TO).
    type, public :: ibuffer_directory_abbrev_type
        character(len=256) :: from = ""
        character(len=256) :: to = ""
    end type ibuffer_directory_abbrev_type

    !> The kinds of node of an `ibuffer_filter_type': a simple filter
    !> specification (SYMBOL . QUALIFIER), (not FILTER-SPEC), (and
    !> FILTER-SPECS...), (or FILTER-SPECS...) and (saved . "NAME").
    integer, parameter, public :: IBUFFER_FILTER_SIMPLE = 1, IBUFFER_FILTER_NOT = 2, IBUFFER_FILTER_AND = 3, &
                                  IBUFFER_FILTER_OR = 4, IBUFFER_FILTER_SAVED = 5

    !> A node of a filter specification, in prefix order: its KIND, the
    !> number of operands (COUNT) of `not', `and' and `or' (which follow
    !> it), the NAME of a simple filter (such as "mode" or "name"), and its
    !> qualifier: a string (QUALIFIER(1:QUALIFIER_LEN), also the name of a
    !> saved filter), an integer NUMBER, or a PREDICATE.
    type, public :: ibuffer_filter_node_type
        integer :: kind = IBUFFER_FILTER_SIMPLE
        integer :: count = 0
        character(len=IBUFFER_NAME_LEN) :: name = ""
        logical :: has_qualifier = .false.
        character(len=256) :: qualifier = ""
        integer :: qualifier_len = 0
        logical :: is_number = .false.
        integer(int64) :: number = 0
        procedure(ibuffer_buffer_predicate_proc), pointer, nopass :: predicate => null()
    end type ibuffer_filter_node_type

    !> A filter specification, made with `ibuffer_and', `ibuffer_or',
    !> `ibuffer_not', `ibuffer_saved' and the simple filters
    !> (`ibuffer_major_mode', `ibuffer_name', ...): its nodes in prefix
    !> order (the tree flattened).
    type, public :: ibuffer_filter_type
        type(ibuffer_filter_node_type), allocatable :: nodes(:)
    end type ibuffer_filter_type

    !> A named list of filter specifications, joined with an implicit
    !> logical `and' (an element ("NAME" . FILTER-LIST) of
    !> `ibuffer_saved_filters', or ("LABEL" FILTER-SPECS...) of
    !> `ibuffer_filter_groups').
    type, public :: ibuffer_saved_filter_type
        character(len=:), allocatable :: name
        type(ibuffer_filter_type), allocatable :: filters(:)
    end type ibuffer_saved_filter_type

    !> The saved filters (the type of `ibuffer_saved_filters').
    type, public :: ibuffer_saved_filters_type
        type(ibuffer_saved_filter_type), allocatable :: entries(:)
    contains
        !> Save the filters F1, F2, ... as NAME (replacing a saved filter
        !> of that name, otherwise added last).
        procedure :: add => saved_filters_add
        !> Save FILTERS as NAME; FRONT true adds a new one first.
        procedure :: set => saved_filters_set
        !> The index of the saved filter NAME (0 for none).
        procedure :: find => saved_filters_find
        !> Remove the saved filter NAME.
        procedure :: remove => saved_filters_remove
        !> Remove all the saved filters.
        procedure :: clear => saved_filters_clear
        !> The number of saved filters.
        procedure :: count => saved_filters_count
    end type ibuffer_saved_filters_type

    !> Filter groups, in order: each a label and filter specifications
    !> (the type of `ibuffer_filter_groups').
    type, public :: ibuffer_filter_groups_type
        type(ibuffer_saved_filter_type), allocatable :: groups(:)
    contains
        !> Add the group NAME with the filters F1, F2, ... last.
        procedure :: add => filter_groups_add
        !> Insert the group NAME with FILTERS at position AT.
        procedure :: insert => filter_groups_insert
        !> The index of the group NAME (0 for none).
        procedure :: find => filter_groups_find
        !> Remove the group NAME.
        procedure :: remove => filter_groups_remove
        !> Remove all the groups.
        procedure :: clear => filter_groups_clear
        !> The number of groups.
        procedure :: count => filter_groups_count
    end type ibuffer_filter_groups_type

    !> A named set of filter groups (an element ("NAME" .
    !> FILTER-GROUP-LIST) of `ibuffer_saved_filter_groups').
    type, public :: ibuffer_named_groups_type
        character(len=:), allocatable :: name
        type(ibuffer_filter_groups_type) :: groups
    end type ibuffer_named_groups_type

    !> The saved sets of filter groups (the type of
    !> `ibuffer_saved_filter_groups').
    type, public :: ibuffer_saved_filter_groups_type
        type(ibuffer_named_groups_type), allocatable :: entries(:)
    contains
        !> Save GROUPS as NAME (replacing a set of that name, otherwise
        !> added last, or first with FRONT).
        procedure :: add => saved_groups_add
        !> The index of the set NAME (0 for none).
        procedure :: find => saved_groups_find
        !> Remove the set NAME.
        procedure :: remove => saved_groups_remove
        !> Remove all the sets.
        procedure :: clear => saved_groups_clear
        !> The number of sets.
        procedure :: count => saved_groups_count
    end type ibuffer_saved_filter_groups_type

    abstract interface
        !> A filter's BODY: whether BUF should be displayed, given the
        !> QUALIFIER of the filter specification (`define-ibuffer-filter').
        recursive logical function ibuffer_filter_test_proc(buf, qualifier, status) result(included)
            import :: handle_type, status_type, ibuffer_filter_node_type
            implicit none(type, external)
            type(handle_type), intent(in) :: buf
            type(ibuffer_filter_node_type), intent(in) :: qualifier
            type(status_type), intent(out) :: status
        end function ibuffer_filter_test_proc
    end interface

    public :: ibuffer_filter_test_proc

    !> A filter: its NAME (the SYMBOL of its filter specifications), a
    !> short DESCRIPTION, and the TEST of a buffer.  Port of
    !> ibuf-macs.el:define-ibuffer-filter (an element of
    !> `ibuffer-filtering-alist').
    type, public :: ibuffer_filter_def_type
        character(len=IBUFFER_NAME_LEN) :: name = ""
        character(len=IBUFFER_TEXT_LEN) :: description = ""
        procedure(ibuffer_filter_test_proc), pointer, nopass :: test => null()
    end type ibuffer_filter_def_type

    !> An element (FILTER (FORMAT FORMAT ...)) of
    !> `ibuffer_filter_format_alist'.
    type, public :: ibuffer_filter_format_type
        character(len=IBUFFER_NAME_LEN) :: filter = ""
        type(ibuffer_formats_type) :: formats
    end type ibuffer_filter_format_type

    !> The special formats used when a filter is active (the type of
    !> `ibuffer_filter_format_alist').
    type, public :: ibuffer_filter_formats_type
        type(ibuffer_filter_format_type), allocatable :: entries(:)
    contains
        !> Use FORMATS while the filter FILTER is active.
        procedure :: add => filter_formats_add
        !> Remove all the entries.
        procedure :: clear => filter_formats_clear
        !> The index of the entry of FILTER (0 for none).
        procedure :: find => filter_formats_find
    end type ibuffer_filter_formats_type

contains

    !> A column entry of a format: the column NAME, limited to between
    !> MIN_SIZE and MAX_SIZE characters (0 and -1 by default), aligned by
    !> ALIGN (IBUFFER_ALIGN_LEFT by default) and elided if ELIDE.
    function ibuffer_column(name, min_size, max_size, align, elide, min_function, max_function) result(spec)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: min_size, max_size, align
        logical, intent(in), optional :: elide
        procedure(ibuffer_size_proc), optional :: min_function, max_function
        type(ibuffer_column_spec_type) :: spec

        spec%name = name
        if (present(min_size)) spec%min_size = min_size
        if (present(max_size)) spec%max_size = max_size
        if (present(align)) spec%align = align
        if (present(elide)) spec%elide = elide
        if (present(min_function)) spec%min_function => min_function
        if (present(max_function)) spec%max_function => max_function
    end function ibuffer_column

    !> A literal string entry of a format: TEXT.
    function ibuffer_text(text) result(spec)
        character(len=*), intent(in) :: text
        type(ibuffer_column_spec_type) :: spec

        spec%text = text
        spec%text_len = min(len(text), IBUFFER_TEXT_LEN)
    end function ibuffer_text

    subroutine formats_add(self, entries)
        class(ibuffer_formats_type), intent(inout) :: self
        type(ibuffer_column_spec_type), intent(in) :: entries(:)
        type(ibuffer_format_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(self%formats)) allocate (self%formats(0))
        n = size(self%formats)
        allocate (grown(n + 1))

        do k = 1, n
            call move_alloc(self%formats(k)%entries, grown(k)%entries)
        end do

        allocate (grown(n + 1)%entries(size(entries)))
        grown(n + 1)%entries = entries
        call move_alloc(grown, self%formats)
    end subroutine formats_add

    subroutine formats_clear(self)
        class(ibuffer_formats_type), intent(inout) :: self

        if (allocated(self%formats)) deallocate (self%formats)
        allocate (self%formats(0))
    end subroutine formats_clear

    integer function formats_count(self) result(n)
        class(ibuffer_formats_type), intent(in) :: self

        n = 0
        if (allocated(self%formats)) n = size(self%formats)
    end function formats_count

    ! ------------------------------------------------------------------
    ! Filters
    ! ------------------------------------------------------------------

    !> A compound filter specification: the logical-and of the filters F1, F2, ... (one or more, up to 16): (and FILTER-SPECS...).
    function ibuffer_and(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16) result(f)
        type(ibuffer_filter_type), intent(in) :: f1
        type(ibuffer_filter_type), intent(in), optional :: f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, &
                                                           f15, f16
        type(ibuffer_filter_type) :: f
        integer :: count

        allocate (f%nodes(1))
        f%nodes(1)%kind = IBUFFER_FILTER_AND
        count = 1
        call append_nodes(f, f1)

        if (present(f2)) then
            call append_nodes(f, f2)
            count = count + 1
        end if

        if (present(f3)) then
            call append_nodes(f, f3)
            count = count + 1
        end if

        if (present(f4)) then
            call append_nodes(f, f4)
            count = count + 1
        end if

        if (present(f5)) then
            call append_nodes(f, f5)
            count = count + 1
        end if

        if (present(f6)) then
            call append_nodes(f, f6)
            count = count + 1
        end if

        if (present(f7)) then
            call append_nodes(f, f7)
            count = count + 1
        end if

        if (present(f8)) then
            call append_nodes(f, f8)
            count = count + 1
        end if

        if (present(f9)) then
            call append_nodes(f, f9)
            count = count + 1
        end if

        if (present(f10)) then
            call append_nodes(f, f10)
            count = count + 1
        end if

        if (present(f11)) then
            call append_nodes(f, f11)
            count = count + 1
        end if

        if (present(f12)) then
            call append_nodes(f, f12)
            count = count + 1
        end if

        if (present(f13)) then
            call append_nodes(f, f13)
            count = count + 1
        end if

        if (present(f14)) then
            call append_nodes(f, f14)
            count = count + 1
        end if

        if (present(f15)) then
            call append_nodes(f, f15)
            count = count + 1
        end if

        if (present(f16)) then
            call append_nodes(f, f16)
            count = count + 1
        end if

        f%nodes(1)%count = count
    end function ibuffer_and

    !> A compound filter specification: the logical-or of the filters F1, F2, ... (one or more, up to 16): (or FILTER-SPECS...).
    function ibuffer_or(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16) result(f)
        type(ibuffer_filter_type), intent(in) :: f1
        type(ibuffer_filter_type), intent(in), optional :: f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, &
                                                           f15, f16
        type(ibuffer_filter_type) :: f
        integer :: count

        allocate (f%nodes(1))
        f%nodes(1)%kind = IBUFFER_FILTER_OR
        count = 1
        call append_nodes(f, f1)

        if (present(f2)) then
            call append_nodes(f, f2)
            count = count + 1
        end if

        if (present(f3)) then
            call append_nodes(f, f3)
            count = count + 1
        end if

        if (present(f4)) then
            call append_nodes(f, f4)
            count = count + 1
        end if

        if (present(f5)) then
            call append_nodes(f, f5)
            count = count + 1
        end if

        if (present(f6)) then
            call append_nodes(f, f6)
            count = count + 1
        end if

        if (present(f7)) then
            call append_nodes(f, f7)
            count = count + 1
        end if

        if (present(f8)) then
            call append_nodes(f, f8)
            count = count + 1
        end if

        if (present(f9)) then
            call append_nodes(f, f9)
            count = count + 1
        end if

        if (present(f10)) then
            call append_nodes(f, f10)
            count = count + 1
        end if

        if (present(f11)) then
            call append_nodes(f, f11)
            count = count + 1
        end if

        if (present(f12)) then
            call append_nodes(f, f12)
            count = count + 1
        end if

        if (present(f13)) then
            call append_nodes(f, f13)
            count = count + 1
        end if

        if (present(f14)) then
            call append_nodes(f, f14)
            count = count + 1
        end if

        if (present(f15)) then
            call append_nodes(f, f15)
            count = count + 1
        end if

        if (present(f16)) then
            call append_nodes(f, f16)
            count = count + 1
        end if

        f%nodes(1)%count = count
    end function ibuffer_or

    !> A compound filter specification: the logical complement of F
    !> (not FILTER-SPEC).
    function ibuffer_not(f1) result(f)
        type(ibuffer_filter_type), intent(in) :: f1
        type(ibuffer_filter_type) :: f

        allocate (f%nodes(1))
        f%nodes(1)%kind = IBUFFER_FILTER_NOT
        f%nodes(1)%count = 1
        call append_nodes(f, f1)
    end function ibuffer_not

    !> The filter saved under the string NAME in `ibuffer_saved_filters'
    !> (saved . "NAME").
    function ibuffer_saved(name) result(f)
        character(len=*), intent(in) :: name
        type(ibuffer_filter_type) :: f

        allocate (f%nodes(1))
        f%nodes(1)%kind = IBUFFER_FILTER_SAVED
        call set_string(f%nodes(1), name)
    end function ibuffer_saved

    !> A simple filter specification (SYMBOL . QUALIFIER): the filter NAME
    !> of `ibuffer_filtering_alist' with the string QUALIFIER, the integer
    !> NUMBER or the PREDICATE (none for a filter without qualifier).
    function ibuffer_filter(name, qualifier, number, predicate) result(f)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: qualifier
        integer(int64), intent(in), optional :: number
        procedure(ibuffer_buffer_predicate_proc), optional :: predicate
        type(ibuffer_filter_type) :: f

        allocate (f%nodes(1))
        f%nodes(1)%kind = IBUFFER_FILTER_SIMPLE
        f%nodes(1)%name = name
        if (present(qualifier)) call set_string(f%nodes(1), qualifier)

        if (present(number)) then
            f%nodes(1)%has_qualifier = .true.
            f%nodes(1)%is_number = .true.
            f%nodes(1)%number = number
        end if

        if (present(predicate)) then
            f%nodes(1)%has_qualifier = .true.
            f%nodes(1)%predicate => predicate
        end if
    end function ibuffer_filter

    subroutine set_string(node, s)
        type(ibuffer_filter_node_type), intent(inout) :: node
        character(len=*), intent(in) :: s

        node%has_qualifier = .true.
        node%qualifier = s
        node%qualifier_len = min(len(s), len(node%qualifier))
    end subroutine set_string

    !> Buffers whose major mode is MODE (mode . MODE).
    function ibuffer_major_mode(mode) result(f)
        character(len=*), intent(in) :: mode
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("mode", qualifier=mode)
    end function ibuffer_major_mode

    !> Buffers whose major mode, one in use, is MODE (used-mode . MODE).
    function ibuffer_used_mode(mode) result(f)
        character(len=*), intent(in) :: mode
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("used_mode", qualifier=mode)
    end function ibuffer_used_mode

    !> Buffers whose major mode derives from MODE (derived-mode . MODE).
    function ibuffer_derived_mode(mode) result(f)
        character(len=*), intent(in) :: mode
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("derived_mode", qualifier=mode)
    end function ibuffer_derived_mode

    !> Buffers whose name matches REGEXP (name . REGEXP).
    function ibuffer_name(regexp) result(f)
        character(len=*), intent(in) :: regexp
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("name", qualifier=regexp)
    end function ibuffer_name

    !> Buffers running a process (process).
    function ibuffer_process() result(f)
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("process")
    end function ibuffer_process

    !> Buffers whose name begins and ends with * (starred-name).
    function ibuffer_starred_name() result(f)
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("starred_name")
    end function ibuffer_starred_name

    !> Buffers whose full file name matches REGEXP (filename . REGEXP).
    function ibuffer_filename(regexp) result(f)
        character(len=*), intent(in) :: regexp
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("filename", qualifier=regexp)
    end function ibuffer_filename

    !> Buffers whose file basename matches REGEXP (basename . REGEXP).
    function ibuffer_basename(regexp) result(f)
        character(len=*), intent(in) :: regexp
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("basename", qualifier=regexp)
    end function ibuffer_basename

    !> Buffers whose file name extension matches REGEXP (file-extension .
    !> REGEXP).
    function ibuffer_file_extension(regexp) result(f)
        character(len=*), intent(in) :: regexp
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("file_extension", qualifier=regexp)
    end function ibuffer_file_extension

    !> Buffers whose directory matches REGEXP (directory . REGEXP).
    function ibuffer_directory(regexp) result(f)
        character(len=*), intent(in) :: regexp
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("directory", qualifier=regexp)
    end function ibuffer_directory

    !> Buffers bigger than SIZE (size-gt . SIZE).
    function ibuffer_size_gt(size) result(f)
        integer(int64), intent(in) :: size
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("size_gt", number=size)
    end function ibuffer_size_gt

    !> Buffers smaller than SIZE (size-lt . SIZE).
    function ibuffer_size_lt(size) result(f)
        integer(int64), intent(in) :: size
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("size_lt", number=size)
    end function ibuffer_size_lt

    !> Modified buffers (modified).
    function ibuffer_modified() result(f)
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("modified")
    end function ibuffer_modified

    !> Buffers visiting a file (visiting-file).
    function ibuffer_visiting_file() result(f)
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("visiting_file")
    end function ibuffer_visiting_file

    !> Buffers whose contents match REGEXP (content . REGEXP).
    function ibuffer_content(regexp) result(f)
        character(len=*), intent(in) :: regexp
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("content", qualifier=regexp)
    end function ibuffer_content

    !> Buffers for which PREDICATE, called with the buffer current, returns
    !> true (predicate . FORM).
    function ibuffer_predicate(predicate) result(f)
        procedure(ibuffer_buffer_predicate_proc) :: predicate
        type(ibuffer_filter_type) :: f

        f = ibuffer_filter("predicate", predicate=predicate)
    end function ibuffer_predicate

    ! Append the nodes of FROM to F.
    subroutine append_nodes(f, from)
        type(ibuffer_filter_type), intent(inout) :: f
        type(ibuffer_filter_type), intent(in) :: from
        type(ibuffer_filter_node_type), allocatable :: grown(:)
        integer :: n, m

        n = size(f%nodes)
        m = 0
        if (allocated(from%nodes)) m = size(from%nodes)
        allocate (grown(n + m))
        grown(1:n) = f%nodes
        if (m > 0) grown(n + 1:n + m) = from%nodes
        call move_alloc(grown, f%nodes)
    end subroutine append_nodes

    !> The number of nodes of the filter starting at node K of F (the node
    !> and its operands).
    recursive integer function ibuffer_filter_extent(f, k) result(n)
        type(ibuffer_filter_type), intent(in) :: f
        integer, intent(in) :: k
        integer :: j, c

        n = 1

        select case (f%nodes(k)%kind)
        case (IBUFFER_FILTER_NOT, IBUFFER_FILTER_AND, IBUFFER_FILTER_OR)
            do c = 1, f%nodes(k)%count
                j = k + n
                n = n + ibuffer_filter_extent(f, j)
            end do
        case default
            ! (A simple or saved filter has no operands.)
        end select
    end function ibuffer_filter_extent

    !> The filter of the nodes K..K+N-1 of F (an operand).
    function ibuffer_filter_part(f, k, n) result(part)
        type(ibuffer_filter_type), intent(in) :: f
        integer, intent(in) :: k, n
        type(ibuffer_filter_type) :: part

        allocate (part%nodes(n))
        part%nodes = f%nodes(k:k + n - 1)
    end function ibuffer_filter_part

    !> Whether filters A and B are the same specification (`equal').
    logical function ibuffer_filter_equal(a, b) result(same)
        type(ibuffer_filter_type), intent(in) :: a, b
        integer :: k

        same = .false.
        if (.not. allocated(a%nodes) .or. .not. allocated(b%nodes)) return
        if (size(a%nodes) /= size(b%nodes)) return

        do k = 1, size(a%nodes)
            associate (x => a%nodes(k), y => b%nodes(k))
                if (x%kind /= y%kind .or. x%count /= y%count .or. x%name /= y%name .or. &
                    (x%has_qualifier .neqv. y%has_qualifier) .or. x%qualifier_len /= y%qualifier_len .or. &
                    x%qualifier /= y%qualifier .or. (x%is_number .neqv. y%is_number) .or. x%number /= y%number) return
                if (associated(x%predicate) .neqv. associated(y%predicate)) return

                if (associated(x%predicate)) then
                    if (.not. associated(x%predicate, y%predicate)) return
                end if
            end associate
        end do

        same = .true.
    end function ibuffer_filter_equal

    ! Add a list of filters F1, F2, ... to FILTERS.
    subroutine add_filters(filters, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
        type(ibuffer_filter_type), allocatable, intent(out) :: filters(:)
        type(ibuffer_filter_type), intent(in), optional :: f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, &
                                                           f14, f15, f16
        integer :: n

        allocate (filters(16))
        n = 0
        call put(f1)
        call put(f2)
        call put(f3)
        call put(f4)
        call put(f5)
        call put(f6)
        call put(f7)
        call put(f8)
        call put(f9)
        call put(f10)
        call put(f11)
        call put(f12)
        call put(f13)
        call put(f14)
        call put(f15)
        call put(f16)
        filters = filters(1:n)

    contains

        subroutine put(f)
            type(ibuffer_filter_type), intent(in), optional :: f

            if (.not. present(f)) return
            n = n + 1
            filters(n) = f
        end subroutine put
    end subroutine add_filters

    subroutine saved_filters_add(self, name, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
        class(ibuffer_saved_filters_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        type(ibuffer_filter_type), intent(in), optional :: f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, &
                                                           f14, f15, f16
        type(ibuffer_filter_type), allocatable :: filters(:)

        call add_filters(filters, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
        call self%set(name, filters)
    end subroutine saved_filters_add

    subroutine saved_filters_set(self, name, filters, front)
        class(ibuffer_saved_filters_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        type(ibuffer_filter_type), intent(in) :: filters(:)
        logical, intent(in), optional :: front
        type(ibuffer_saved_filter_type), allocatable :: grown(:)
        integer :: k, n
        logical :: at_front

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        k = self%find(name)

        if (k == 0) then
            at_front = .false.
            if (present(front)) at_front = front
            n = size(self%entries)
            allocate (grown(n + 1))

            if (at_front) then
                do k = 1, n
                    call move_saved(self%entries(k), grown(k + 1))
                end do

                k = 1
            else
                do k = 1, n
                    call move_saved(self%entries(k), grown(k))
                end do

                k = n + 1
            end if

            call move_alloc(grown, self%entries)
        end if

        self%entries(k)%name = name
        if (allocated(self%entries(k)%filters)) deallocate (self%entries(k)%filters)
        allocate (self%entries(k)%filters(size(filters)))
        self%entries(k)%filters = filters
    end subroutine saved_filters_set

    subroutine move_saved(from, to)
        type(ibuffer_saved_filter_type), intent(inout) :: from, to

        call move_alloc(from%name, to%name)
        call move_alloc(from%filters, to%filters)
    end subroutine move_saved

    integer function saved_filters_find(self, name) result(k)
        class(ibuffer_saved_filters_type), intent(in) :: self
        character(len=*), intent(in) :: name

        if (allocated(self%entries)) then
            do k = 1, size(self%entries)
                if (self%entries(k)%name == name .and. len(self%entries(k)%name) == len(name)) return
            end do
        end if

        k = 0
    end function saved_filters_find

    subroutine saved_filters_remove(self, name)
        class(ibuffer_saved_filters_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        type(ibuffer_saved_filter_type), allocatable :: kept(:)
        integer :: k, n

        if (.not. allocated(self%entries)) return
        allocate (kept(size(self%entries)))
        n = 0

        do k = 1, size(self%entries)
            if (self%entries(k)%name == name .and. len(self%entries(k)%name) == len(name)) cycle
            n = n + 1
            call move_saved(self%entries(k), kept(n))
        end do

        deallocate (self%entries)
        allocate (self%entries(n))

        do k = 1, n
            call move_saved(kept(k), self%entries(k))
        end do
    end subroutine saved_filters_remove

    subroutine saved_filters_clear(self)
        class(ibuffer_saved_filters_type), intent(inout) :: self

        if (allocated(self%entries)) deallocate (self%entries)
        allocate (self%entries(0))
    end subroutine saved_filters_clear

    integer function saved_filters_count(self) result(n)
        class(ibuffer_saved_filters_type), intent(in) :: self

        n = 0
        if (allocated(self%entries)) n = size(self%entries)
    end function saved_filters_count

    subroutine filter_groups_add(self, name, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
        class(ibuffer_filter_groups_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        type(ibuffer_filter_type), intent(in), optional :: f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, &
                                                           f14, f15, f16
        type(ibuffer_filter_type), allocatable :: filters(:)

        call add_filters(filters, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
        call self%insert(name, filters, size_of_groups(self) + 1)
    end subroutine filter_groups_add

    integer function size_of_groups(groups) result(n)
        class(ibuffer_filter_groups_type), intent(in) :: groups

        n = 0
        if (allocated(groups%groups)) n = size(groups%groups)
    end function size_of_groups

    subroutine filter_groups_insert(self, name, filters, at)
        class(ibuffer_filter_groups_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        type(ibuffer_filter_type), intent(in) :: filters(:)
        integer, intent(in) :: at
        type(ibuffer_saved_filter_type), allocatable :: grown(:)
        integer :: k, n, pos

        if (.not. allocated(self%groups)) allocate (self%groups(0))
        n = size(self%groups)
        pos = max(1, min(at, n + 1))
        allocate (grown(n + 1))

        do k = 1, pos - 1
            call move_saved(self%groups(k), grown(k))
        end do

        grown(pos)%name = name
        allocate (grown(pos)%filters(size(filters)))
        grown(pos)%filters = filters

        do k = pos, n
            call move_saved(self%groups(k), grown(k + 1))
        end do

        call move_alloc(grown, self%groups)
    end subroutine filter_groups_insert

    integer function filter_groups_find(self, name) result(k)
        class(ibuffer_filter_groups_type), intent(in) :: self
        character(len=*), intent(in) :: name

        if (allocated(self%groups)) then
            do k = 1, size(self%groups)
                if (self%groups(k)%name == name .and. len(self%groups(k)%name) == len(name)) return
            end do
        end if

        k = 0
    end function filter_groups_find

    subroutine filter_groups_remove(self, name)
        class(ibuffer_filter_groups_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        type(ibuffer_saved_filter_type), allocatable :: kept(:)
        integer :: k, n

        if (.not. allocated(self%groups)) return
        allocate (kept(size(self%groups)))
        n = 0

        do k = 1, size(self%groups)
            if (self%groups(k)%name == name .and. len(self%groups(k)%name) == len(name)) cycle
            n = n + 1
            call move_saved(self%groups(k), kept(n))
        end do

        deallocate (self%groups)
        allocate (self%groups(n))

        do k = 1, n
            call move_saved(kept(k), self%groups(k))
        end do
    end subroutine filter_groups_remove

    subroutine filter_groups_clear(self)
        class(ibuffer_filter_groups_type), intent(inout) :: self

        if (allocated(self%groups)) deallocate (self%groups)
        allocate (self%groups(0))
    end subroutine filter_groups_clear

    integer function filter_groups_count(self) result(n)
        class(ibuffer_filter_groups_type), intent(in) :: self

        n = size_of_groups(self)
    end function filter_groups_count

    subroutine saved_groups_add(self, name, groups, front)
        class(ibuffer_saved_filter_groups_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        type(ibuffer_filter_groups_type), intent(in) :: groups
        logical, intent(in), optional :: front
        type(ibuffer_named_groups_type), allocatable :: grown(:)
        integer :: k, n
        logical :: at_front

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        k = self%find(name)

        if (k == 0) then
            at_front = .false.
            if (present(front)) at_front = front
            n = size(self%entries)
            allocate (grown(n + 1))

            if (at_front) then
                do k = 1, n
                    call move_named(self%entries(k), grown(k + 1))
                end do

                k = 1
            else
                do k = 1, n
                    call move_named(self%entries(k), grown(k))
                end do

                k = n + 1
            end if

            call move_alloc(grown, self%entries)
        end if

        self%entries(k)%name = name
        self%entries(k)%groups = groups
    end subroutine saved_groups_add

    subroutine move_named(from, to)
        type(ibuffer_named_groups_type), intent(inout) :: from, to

        call move_alloc(from%name, to%name)
        call move_alloc(from%groups%groups, to%groups%groups)
    end subroutine move_named

    integer function saved_groups_find(self, name) result(k)
        class(ibuffer_saved_filter_groups_type), intent(in) :: self
        character(len=*), intent(in) :: name

        if (allocated(self%entries)) then
            do k = 1, size(self%entries)
                if (self%entries(k)%name == name .and. len(self%entries(k)%name) == len(name)) return
            end do
        end if

        k = 0
    end function saved_groups_find

    subroutine saved_groups_remove(self, name)
        class(ibuffer_saved_filter_groups_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        type(ibuffer_named_groups_type), allocatable :: kept(:)
        integer :: k, n

        if (.not. allocated(self%entries)) return
        allocate (kept(size(self%entries)))
        n = 0

        do k = 1, size(self%entries)
            if (self%entries(k)%name == name .and. len(self%entries(k)%name) == len(name)) cycle
            n = n + 1
            call move_named(self%entries(k), kept(n))
        end do

        deallocate (self%entries)
        allocate (self%entries(n))

        do k = 1, n
            call move_named(kept(k), self%entries(k))
        end do
    end subroutine saved_groups_remove

    subroutine saved_groups_clear(self)
        class(ibuffer_saved_filter_groups_type), intent(inout) :: self

        if (allocated(self%entries)) deallocate (self%entries)
        allocate (self%entries(0))
    end subroutine saved_groups_clear

    integer function saved_groups_count(self) result(n)
        class(ibuffer_saved_filter_groups_type), intent(in) :: self

        n = 0
        if (allocated(self%entries)) n = size(self%entries)
    end function saved_groups_count

    subroutine filter_formats_add(self, filter, formats)
        class(ibuffer_filter_formats_type), intent(inout) :: self
        character(len=*), intent(in) :: filter
        type(ibuffer_formats_type), intent(in) :: formats
        type(ibuffer_filter_format_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        n = size(self%entries)
        allocate (grown(n + 1))

        do k = 1, n
            grown(k)%filter = self%entries(k)%filter
            call move_alloc(self%entries(k)%formats%formats, grown(k)%formats%formats)
        end do

        grown(n + 1)%filter = filter
        grown(n + 1)%formats = formats
        call move_alloc(grown, self%entries)
    end subroutine filter_formats_add

    subroutine filter_formats_clear(self)
        class(ibuffer_filter_formats_type), intent(inout) :: self

        if (allocated(self%entries)) deallocate (self%entries)
        allocate (self%entries(0))
    end subroutine filter_formats_clear

    integer function filter_formats_find(self, filter) result(k)
        class(ibuffer_filter_formats_type), intent(in) :: self
        character(len=*), intent(in) :: filter

        if (allocated(self%entries)) then
            do k = 1, size(self%entries)
                if (self%entries(k)%filter == filter) return
            end do
        end if

        k = 0
    end function filter_formats_find
end module ibuf_macs_fm
