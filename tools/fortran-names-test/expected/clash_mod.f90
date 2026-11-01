! A module whose own forward_expression clashes with the rename of the
! forward_sexp it imports: tools/fortran-names.sh leaves it as it is.
module clash_mod
    use expression_motion_mod, only: forward_sexp
    implicit none (type, external)

contains

    subroutine forward_expression()
    end subroutine forward_expression
end module clash_mod
