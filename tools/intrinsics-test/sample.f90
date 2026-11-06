! A fixture for tools/test-intrinsics.sh (not compiled).
module sample
    implicit none (type, external)

    interface
        !> An interface body is no definition.
        logical function nilp(v)
            type(value_type), intent(in) :: v
        end function nilp
    end interface

    !> Named constants for nil and t, and meaningful ones.
    integer, parameter, public :: SCROLL_NIL = 0, SCROLL_T = 1, SCROLL_FIRST_ERROR = 2
    integer, parameter :: CHECK_TRUE = 1, CHECK_FALSE = 0, CHECK_NO = 2
    integer, parameter :: COMPILATION_SCROLL_NO = 0, COMPILATION_SCROLL_YES = 1

    !> An array named with `_vector', and a plural one.
    integer :: colors_vector(8) = [(0, k = 1, 8)], colors(8)

    !> A command type with the component `interactive'.
    type :: command_type
        logical :: interactive = .false.
        logical :: is_command = .false.
    end type command_type

    !> The command loop's state as values, and as its types (A7).
    type(value_type) :: this_command, current_prefix_argument
    type(command_type), pointer :: last_command => null()

    !> Another type may have a component `interactive'.
    type :: other_type
        logical :: interactive = .false.
    end type other_type

    !> A procedure pointer and a binding named with run, and their
    !> execute forms; a noun with run in it is no verb.
    type :: hook_type
        procedure(hook_proc), pointer, nopass :: run => null()
        procedure(hook_proc), pointer, nopass :: execute_at_exit => null()
        integer :: runs = 0
    contains
        procedure :: run_safe => hook_execute_safe
        procedure :: execute => hook_execute
    end type hook_type

    !> Constants named off, on, never, disabled and no limit, and their
    !> vocabulary (R2-Q16, R2-Q17).
    integer, parameter :: DABBREV_OFF = 0, DABBREV_ON = 1, SAVE_ABBREVS_NEVER = 2
    integer, parameter :: KMACRO_REPEAT_DISABLED = 3, RECENTF_NO_LIMIT = -1
    integer, parameter :: DABBREV_NO = 4, RECENTF_MAX_SAVED_ITEMS_UNLIMITED = -2

    !> Interfaces named with _proc and predicate, and with _procedure.
    abstract interface
        subroutine filter_proc(n)
            integer, intent(in) :: n
        end subroutine filter_proc

        logical function completion_predicate(s)
            character(len=*), intent(in) :: s
        end function completion_predicate

        subroutine filter_procedure(n)
            integer, intent(in) :: n
        end subroutine filter_procedure
    end interface

    !> A derived type without _type, and one with it.
    type :: logical_setting
        logical :: value = .false.
    end type logical_setting

    type :: logical_setting_type
        logical :: value = .false.
    end type logical_setting_type

    !> A double underscore, a regexp named _re and _regex, an abbreviated
    !> process and a predicate, and their spelled-out forms.
    integer :: smie__parent, smie_parent_variable
    character(len=8) :: f90_keywords_re, sgml_tag_regex, f90_keywords_regexp
    integer :: proc, process
    logical :: pred, test

contains

    !> A thin wrapper: `.not. allocated(v%a)'.
    pure logical function is_null(v)
        type(value_type), intent(in) :: v

        is_null = .not. allocated(v%a)
    end function is_null

    !> A thin wrapper with a result name and a kind.
    pure function count_of(x) result(n)
        integer, intent(in) :: x(:)
        integer(int64) :: n

        n = size(x, kind=int64)
    end function count_of

    !> Not a wrapper: two statements.
    integer function twice(x)
        integer, intent(in) :: x(:)

        twice = size(x)
        twice = 2*twice
    end function twice

    !> Not a wrapper: a module variable in the arguments.
    integer function global_size()
        global_size = size(table)
    end function global_size

    !> Not a wrapper: a module variable compared in the arguments.
    integer function table_index(name)
        character(len=*), intent(in) :: name

        table_index = findloc(table%name == name, .true., dim=1)
    end function table_index

    !> Not a wrapper: nested intrinsics.
    integer function trimmed_length(s)
        character(len=*), intent(in) :: s

        trimmed_length = len(trim(s))
    end function trimmed_length

    !> A thin wrapper of a value's tag: test `v%a(1)%tag' directly.
    elemental logical function is_integer(v)
        type(value_type), intent(in) :: v

        is_integer = v%a(1)%tag == V_INTEGER
    end function is_integer

    !> A thin wrapper of a value's component: read `v%a(1)%i' directly.
    elemental integer(int64) function value_integer(v)
        type(value_type), intent(in) :: v

        value_integer = v%a(1)%i
    end function value_integer

    !> A Lisp helper name, excused by the fixture's baseline.
    logical function memq(x, list)
        integer, intent(in) :: x, list(:)

        memq = any(list == x)
    end function memq

    !> A Lisp helper name.
    subroutine car(list, v)
        integer, intent(in) :: list(:)
        integer, intent(out) :: v

        v = list(1)
    end subroutine car

    !> Lisp predicate names: one ending in _p, one in p after a type word.
    logical function region_active_p(n)
        integer, intent(in) :: n

        region_active_p = n > 0
    end function region_active_p

    logical function processp(n)
        integer, intent(in) :: n

        processp = n > 1
    end function processp

    !> Not predicates: an is_* name, and a word that only ends in p.
    logical function is_region_active(n)
        integer, intent(in) :: n

        is_region_active = n > 0
    end function is_region_active

    integer function keep_group(n)
        integer, intent(in) :: n

        keep_group = n + 1
    end function keep_group

    !> Procedures that run something with run or call, and with execute.
    subroutine run_with_timer(n)
        integer, intent(in) :: n

        print *, n
    end subroutine run_with_timer

    subroutine call_indent_line_function(n)
        integer, intent(in) :: n

        print *, n
    end subroutine call_indent_line_function

    subroutine execute_with_timer(n)
        integer, intent(in) :: n

        print *, n
    end subroutine execute_with_timer

    integer function excluded_run(n)
        integer, intent(in) :: n

        excluded_run = n
    end function excluded_run

    !> Write wrappers: a write into a local buffer then trim, and one write
    !> into the result; then a number written where it is used (no finding).
    function number_text(n) result(text)
        integer, intent(in) :: n
        character(len=:), allocatable :: text
        character(len=11) :: buffer

        write (buffer, "(i0)") n
        text = trim(buffer)
    end function number_text

    character(len=24) function digits_of(n)
        integer, intent(in) :: n

        write (digits_of, "(i0)") n
    end function digits_of

    function line_message(n) result(text)
        integer, intent(in) :: n
        character(len=:), allocatable :: text
        character(len=32) :: buffer

        write (buffer, '("Line ", i0)') n
        text = trim(buffer)//" of the file"
    end function line_message

    !> A number made text through a helper.
    subroutine show_count(n)
        integer, intent(in) :: n

        print *, "Count: "//number_to_string(n), integer_text(n)
    end subroutine show_count

    !> A string comparison by hand (string-lessp): the strings compare
    !> with `<'.
    pure logical function is_name_less(a, b)
        character(len=*), intent(in) :: a, b
        integer :: k

        do k = 1, min(len(a), len(b))
            if (a(k:k) /= b(k:k)) then
                is_name_less = iachar(a(k:k)) < iachar(b(k:k))
                return
            end if
        end do

        is_name_less = len(a) < len(b)
    end function is_name_less

    !> Not a char loop: the common prefix orders no characters.
    integer function common_prefix(s1, s2) result(n)
        character(len=*), intent(in) :: s1, s2
        integer :: i

        n = 0

        do i = 1, min(len(s1), len(s2))
            if (s1(i:i) /= s2(i:i)) exit
            n = i
        end do
    end function common_prefix

    !> Blank skips forward and backward and one that skips newlines too
    !> (`verify'); then a loop that does more (not the rule's form).
    subroutine skip_blanks(s, k, e)
        character(len=*), intent(in) :: s
        integer, intent(out) :: k, e
        integer :: i

        k = 1

        do while (k <= len(s))
            if (s(k:k) /= " " .and. s(k:k) /= TAB) exit
            k = k + 1
        end do

        e = len(s)

        do while (e >= 1)
            if (s(e:e) /= " " .and. s(e:e) /= achar(9)) exit
            e = e - 1
        end do

        i = 1

        do while (i <= len(s))
            if (s(i:i) /= " " .and. s(i:i) /= TAB .and. s(i:i) /= LF) exit
            i = i + 1
        end do

        do while (i <= len(s))
            if (s(i:i) /= " " .and. s(i:i) /= TAB) exit
            i = i + 1
            k = k + 1
        end do
    end subroutine skip_blanks

    !> A constructor of the dummy (write it where it is used); not one: a
    !> callback that only clears its status, and a constructor of a
    !> module variable.
    function make_string(s) result(v)
        character(len=*), intent(in) :: s
        type(value_type) :: v

        v = value_type(text=s)
    end function make_string

    subroutine ignore(status)
        type(status_type), intent(out) :: status

        status = status_type()
    end subroutine ignore

    function default_value() result(v)
        type(value_type) :: v

        v = value_type(text=default_text)
    end function default_value

    !> A private copy of string-to-number (data_mod holds the port).
    integer function string_to_number(s) result(n)
        character(len=*), intent(in) :: s

        read (s, *) n
    end function string_to_number

    !> A private copy of version<= (basic_subprograms_mod holds the port).
    logical function version_le(a, b)
        character(len=*), intent(in) :: a, b

        version_le = a <= b
    end function version_le

    !> A private copy of delq (pack is the intrinsic).
    subroutine list_delq(list, h)
        integer, allocatable, intent(inout) :: list(:)
        integer, intent(in) :: h

        list = pack(list, list /= h)
    end subroutine list_delq
end module sample
