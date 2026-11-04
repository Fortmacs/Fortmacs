! A fixture for tools/test-intrinsics.sh (not compiled): the module that
! holds the port of string-lessp defines is_string_less without a finding
! of the duplicate-helper rule.
module fns_mod
    implicit none (type, external)

contains

    !> Port of fns.c:Fstring_lessp.
    logical function is_string_less(a, b)
        type(value_type), intent(in) :: a, b

        is_string_less = a%text < b%text
    end function is_string_less
end module fns_mod
