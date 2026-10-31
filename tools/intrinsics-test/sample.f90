! A fixture for tools/test-intrinsics.sh (not compiled).
module sample
    implicit none (type, external)

    interface
        !> An interface body is no definition.
        logical function nilp(v)
            type(value_type), intent(in) :: v
        end function nilp
    end interface

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

    !> Not a wrapper: nested intrinsics.
    integer function trimmed_length(s)
        character(len=*), intent(in) :: s

        trimmed_length = len(trim(s))
    end function trimmed_length

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
end module sample
