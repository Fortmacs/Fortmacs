! A fixture for tools/test-lisp-data.sh: two name-value uses and one
! name dispatch.
module probe_mod
    implicit none (type, external)
contains
    subroutine f(name)
        character(len=*), intent(in) :: name

        v = value_type(name="inhibit_read_only")   ! counted
        w = value_type(name="x")                    ! counted

        select case (name)                          ! counted
        case ("a")
        end select
    end subroutine f
end module probe_mod
