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

    public :: ibuffer_column, ibuffer_text

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

        !> A column's SUMMARIZER: the text to display at the bottom of the
        !> column, from the column's CELLS.
        function ibuffer_summarizer_proc(cells) result(text)
            import :: ibuffer_cell_type
            implicit none(type, external)
            type(ibuffer_cell_type), intent(in) :: cells(:)
            character(len=:), allocatable :: text
        end function ibuffer_summarizer_proc

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
end module ibuf_macs_fm
