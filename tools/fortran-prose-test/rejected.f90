! The fixture of tools/test-fortran-prose.sh: each docstring or message
! below uses rejected terms (tools/fortran-prose), in this order.
module rejected_fm
    implicit none(type, external)
    private

contains

    !> If FLAG is non-nil, do it.
    !> Return nil when there is none.
    !> The variable is void.
    !> Symbol's value as variable.
    !> Attempt to set a constant symbol.
    !> Otherwise signal an error.
    !> A command throws to `exit'.
    !> The caller catches it.
    !> As a condition-case handler does.
    !> The face's property list.
    !> PROPS, a plist.
    !> Its interactive spec reads the region.
    !> Not byte-compiled.
    !> The cons of the two.
    !> A dotted pair.
    !> The car of the list.
    !> A lambda on the hook.
    !> As funcall does.
    !> As setq-local does.
    !> The obarray.
    !> See the Elisp manual.
    !> FROM and TO as a pair.
    subroutine rejected(status)
        integer, intent(out) :: status

        status = 0
        call set_error(status, 4, "Wrong type argument: stringp, nil")
    end subroutine rejected

end module rejected_fm
