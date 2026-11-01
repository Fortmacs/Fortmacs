! The fixture of tools/test-fortran-prose.sh: prose the check accepts.
module accepted_mod
    implicit none(type, external)
    private

contains

    !> If FLAG is .true., do it; FOUND .false. when there is none.
    !> Port of simple.el:kill-region (the text properties `plist' and
    !> PLIST are names; a name_with_nil underscore is an identifier).
    !> See Info node `(elisp)Basic Completion'.
    !> Upstream's `condition-case' is cited as such.
    !> Oldest elements are thrown away.
    !> The process gets a POSIX signal.
    subroutine accepted(status)
        integer, intent(out) :: status

        ! A comment may say nil: only docstrings and messages are checked.
        status = 0
        call set_error(status, 4, "Wrong type argument: expected character(len=*)")
    end subroutine accepted

end module accepted_mod
